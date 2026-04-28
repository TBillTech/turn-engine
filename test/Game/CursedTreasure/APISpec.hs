module Game.CursedTreasure.APISpec where

import Test.Hspec
import Test.QuickCheck

import Game.CursedTreasure.API (createNewGame, getGameSetupPlayers, mkCensoredGameState)
import Game.CursedTreasure.Arbitrary ()
import Game.CursedTreasure.Types
    ( ClueCard (HiddenClue)
    , CensoredGameState
    , GameState (..)
    , PlayerDescription (..)
    , PlayerId
    , PlayerState (..)
    , RaisingTreasureState (..)
    , ToGameState (toGameState)
    , TreasureCard (HiddenTreasure)
    , censorRaisingTreasure
    , mkPlayerId
    )

spec :: Spec
spec = do
    describe "mkPlayerId" $ do
        it "accepts only player ids from 1 through 4" $ do
            map mkPlayerId [0 .. 5]
                `shouldBe`
                [ Nothing
                , mkPlayerId 1
                , mkPlayerId 2
                , mkPlayerId 3
                , mkPlayerId 4
                , Nothing
                ]

    describe "createNewGame" $ do
        it "creates a GameState with the requested number of players for seed 12345" $ do
            assertCreateNewGamePlayerCount 3 12345

        it "creates a GameState with the requested number of players for arbitrary seeds" $
            property $ forAll validPlayerCountGen $ \playerCount ->
                forAll arbitrary $ \randomSeed ->
                    counterexample (seedCounterexample playerCount randomSeed) $
                        createNewGamePlayerCountMatches playerCount randomSeed

        it "returns one censored state per player for seed 12345" $ do
            assertCreateNewGameCensoredCount 4 12345

        it "returns one censored state per player for arbitrary seeds" $
            property $ forAll validPlayerCountGen $ \playerCount ->
                forAll arbitrary $ \randomSeed ->
                    counterexample (seedCounterexample playerCount randomSeed) $
                        createNewGameCensoredCountMatches playerCount randomSeed

        it "returns censored states in the same player id order as the created players for seed 12345" $ do
            assertCreateNewGameCensoredOrder 2 12345

        it "returns censored states in the same player id order as the created players for arbitrary seeds" $
            property $ forAll validPlayerCountGen $ \playerCount ->
                forAll arbitrary $ \randomSeed ->
                    counterexample (seedCounterexample playerCount randomSeed) $
                        createNewGameCensoredOrderMatches playerCount randomSeed

    describe "censorRaisingTreasure" $ do
        it "keeps the top treasure visible and hides the rest when nobody is viewing" $
            property $ \drawPile discardPile ->
                let raisingTreasureState =
                        RaisingTreasureState
                            { rtTreasureChest = (drawPile, discardPile)
                            , rtOrder = []
                            , rtPlayerIndex = 0
                            , rtViewing = []
                            }
                    censoredDrawPile = fst (censorRaisingTreasure raisingTreasureState).rtTreasureChest
                 in counterexample ("drawPile=" <> show drawPile) $
                        censoredDrawPile === censorVisibleTopOnly drawPile

        it "hides the full draw pile while treasure cards are being viewed" $
            property $ \drawPile discardPile ->
                let raisingTreasureState =
                        RaisingTreasureState
                            { rtTreasureChest = (drawPile, discardPile)
                            , rtOrder = []
                            , rtPlayerIndex = 0
                            , rtViewing = [mkExistingPlayerId 1]
                            }
                    censoredDrawPile = fst (censorRaisingTreasure raisingTreasureState).rtTreasureChest
                 in counterexample ("drawPile=" <> show drawPile) $
                        censoredDrawPile === map (const HiddenTreasure) drawPile

    describe "censorHiddenInfo" $ do
        it "preserves all non-censored data" $
            property $ \viewerId gameState ->
                let censoredState = toGameState $ mkCensoredGameState gameState viewerId
                 in truncateCensoredFields viewerId censoredState
                        === truncateCensoredFields viewerId gameState

        it "replaces every censored field with hidden values" $
            property $ \viewerId gameState ->
                let censoredState = toGameState $ mkCensoredGameState gameState viewerId
                 in conjoin
                        [ counterexample "other players' clues must be hidden"
                            (all (playerFieldsHidden viewerId) censoredState.players)
                        , counterexample "clue draw pile must be hidden"
                            (all (== HiddenClue) (fst censoredState.clueDeck))
                        , counterexample "treasure draw pile must be hidden"
                            (all (== HiddenTreasure) (fst censoredState.treasureDeck))
                        , counterexample "raising treasure chest must be hidden"
                            (maybe True raisingTreasureHidden censoredState.raisingTreasure)
                        , counterexample "seed must be cleared"
                            (censoredState.seed == (0, 0))
                        ]

truncateCensoredFields :: PlayerId -> GameState -> GameState
truncateCensoredFields viewerId gameState =
    gameState
        { players = map (truncateOther viewerId) gameState.players
        , clueDeck = ([], snd gameState.clueDeck)
        , treasureDeck = ([], snd gameState.treasureDeck)
        , raisingTreasure = truncateRaisingTreasure <$> gameState.raisingTreasure
        , seed = (0, 0)
        }
  where
    truncateOther viewId playerState
        | playerState.player.playerId == viewId = playerState
        | otherwise =
            playerState
                { clues = []
                , viewingTreasures = []
                }

    truncateRaisingTreasure raisingTreasureState =
        raisingTreasureState
            { rtTreasureChest = (truncateRaisingTreasureDrawPile raisingTreasureState, snd raisingTreasureState.rtTreasureChest)
            }

    truncateRaisingTreasureDrawPile raisingTreasureState =
        if null raisingTreasureState.rtViewing
            then take 1 drawPile
            else []
      where
        (drawPile, _) = raisingTreasureState.rtTreasureChest

playerFieldsHidden :: PlayerId -> PlayerState -> Bool
playerFieldsHidden viewerId playerState
    | playerState.player.playerId == viewerId = True
    | otherwise =
        all (== HiddenClue) playerState.clues
            && all (== HiddenTreasure) playerState.viewingTreasures

raisingTreasureHidden :: RaisingTreasureState -> Bool
raisingTreasureHidden raisingTreasureState 
    | null raisingTreasureState.rtViewing = notElem HiddenTreasure (take 1 drawPile) && all (== HiddenTreasure) (drop 1 drawPile)
    | otherwise = all (== HiddenTreasure) drawPile
  where
    (drawPile, _) = raisingTreasureState.rtTreasureChest

validPlayerCountGen :: Gen Int
validPlayerCountGen = elements [2, 3, 4]

seedCounterexample :: Int -> Int -> String
seedCounterexample playerCount randomSeed =
    "playerCount=" <> show playerCount <> ", seed=" <> show randomSeed

mkExistingPlayerId :: Int -> PlayerId
mkExistingPlayerId playerNumber =
    case mkPlayerId playerNumber of
        Just playerId -> playerId
        Nothing -> error $ "Expected valid player id, got " <> show playerNumber

createNewGamePlayerCountMatches :: Int -> Int -> Bool
createNewGamePlayerCountMatches playerCount randomSeed =
    length gameState.players == length requestedPlayers
  where
    requestedPlayers = take playerCount getGameSetupPlayers
    (gameState, _) = createNewGame requestedPlayers randomSeed

createNewGameCensoredCountMatches :: Int -> Int -> Bool
createNewGameCensoredCountMatches playerCount randomSeed =
    length censoredStates == length requestedPlayers
  where
    requestedPlayers = take playerCount getGameSetupPlayers
    (_, censoredStates :: [CensoredGameState]) = createNewGame requestedPlayers randomSeed

createNewGameCensoredOrderMatches :: Int -> Int -> Bool
createNewGameCensoredOrderMatches playerCount randomSeed =
    all (== requestedPlayerIds) censoredPlayerIdLists
  where
    requestedPlayers = take playerCount getGameSetupPlayers
    requestedPlayerIds = map (.playerId) requestedPlayers
    (_, censoredStates) = createNewGame requestedPlayers randomSeed
    censoredPlayerIdLists = map (map (.player.playerId) . (.players) . toGameState) censoredStates

assertCreateNewGamePlayerCount :: Int -> Int -> Expectation
assertCreateNewGamePlayerCount playerCount randomSeed =
    assertSeededExpectation
        (seedCounterexample playerCount randomSeed)
        (createNewGamePlayerCountMatches playerCount randomSeed)

assertCreateNewGameCensoredCount :: Int -> Int -> Expectation
assertCreateNewGameCensoredCount playerCount randomSeed =
    assertSeededExpectation
        (seedCounterexample playerCount randomSeed)
        (createNewGameCensoredCountMatches playerCount randomSeed)

assertCreateNewGameCensoredOrder :: Int -> Int -> Expectation
assertCreateNewGameCensoredOrder playerCount randomSeed =
    assertSeededExpectation
        (seedCounterexample playerCount randomSeed)
        (createNewGameCensoredOrderMatches playerCount randomSeed)

assertSeededExpectation :: String -> Bool -> Expectation
assertSeededExpectation failureContext passed =
    if passed
        then pass
        else expectationFailure failureContext

censorVisibleTopOnly :: [TreasureCard] -> [TreasureCard]
censorVisibleTopOnly [] = []
censorVisibleTopOnly (topCard : rest) = topCard : map (const HiddenTreasure) rest
