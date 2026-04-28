module Game.CursedTreasure.APISpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Random (mkStdGen)
import Test.Hspec
import Test.QuickCheck

import Game.Core.Primitives
    ( CubeCoordinate
    , isUnitCubeDist
    , mkCubeCoordinate
    , radiusOneCubeCoordinates
    , radiusTwoCubeCoordinates
    , toPair
    )
import Game.CursedTreasure.API
    ( createNewGame
    , distanceSet
    , fillHexOcean
    , getGameSetupPlayers
    , getConnectedSets
    , isValidTerrainBoard
    , mkCensoredGameState
    )
import Game.CursedTreasure.Arbitrary ()
import Game.CursedTreasure.Types
    ( ClueCard (HiddenClue)
    , CensoredGameState
    , Feature (..)
    , GameState (..)
    , HexMap
    , PlayerDescription (..)
    , PlayerId
    , PlayerState (..)
    , RaisingTreasureState (..)
    , TerrainHex (..)
    , ToGameState (toGameState)
    , TreasureCard (HiddenTreasure)
    , censorRaisingTreasure
    , mkPlayerId
    )

spec :: Spec
spec = do
    describe "distanceSet" $ do
        it "returns the original seed set for distance 0" $ do
            distanceSet 0 (one origin) `shouldBe` one origin

        it "adds the six adjacent hexes for distance 1" $ do
            distanceSet 1 (one origin) `shouldBe` expectedDistanceOne

        it "includes the full radius-2 ring for distance 2" $ do
            distanceSet 2 (one origin) `shouldBe` expectedDistanceTwo

        it "unions overlapping radius-2 expansions from multiple seed hexes" $ do
            distanceSet 2 overlappingSeeds `shouldBe` expectedOverlappingDistanceTwo

    describe "mkCoordinateBlock" $ do
        it "creates all cube coordinates inside the requested q and r bounds" $ do
            mkCoordinateBlock (0, 1) (-1, 1) `shouldBe`
                [ mkCubeCoordinate 0 (-1)
                , mkCubeCoordinate 0 0
                , mkCubeCoordinate 0 1
                , mkCubeCoordinate 1 (-1)
                , mkCubeCoordinate 1 0
                , mkCubeCoordinate 1 1
                ]

        it "lets adjacent q-rhombus blocks touch without overlapping" $ do
            Map.size testTerrainBoard `shouldBe` 8
            findBoundaryContacts leftBlockCoords rightBlockCoords `shouldBe`
                [ (mkCubeCoordinate 1 0, mkCubeCoordinate 2 0)
                , (mkCubeCoordinate 1 1, mkCubeCoordinate 2 0)
                , (mkCubeCoordinate 1 1, mkCubeCoordinate 2 1)
                ]

        it "lets adjacent 3x3 q-rhombus blocks touch across the shared edge" $ do
            Map.size testTerrainBoard3x3 `shouldBe` 18
            findBoundaryContacts leftBlockCoords3x3 rightBlockCoords3x3 `shouldBe`
                [ (mkCubeCoordinate 2 0, mkCubeCoordinate 3 0)
                , (mkCubeCoordinate 2 1, mkCubeCoordinate 3 0)
                , (mkCubeCoordinate 2 1, mkCubeCoordinate 3 1)
                , (mkCubeCoordinate 2 2, mkCubeCoordinate 3 1)
                , (mkCubeCoordinate 2 2, mkCubeCoordinate 3 2)
                ]

        it "lets a 4x4 block touch a 3x3 block without overlapping" $ do
            Map.size testTerrainBoard4x4And3x3 `shouldBe` 25
            findBoundaryContacts leftBlockCoords4x4 rightBlockCoords3x3Shifted `shouldBe`
                [ (mkCubeCoordinate 3 0, mkCubeCoordinate 4 0)
                , (mkCubeCoordinate 3 1, mkCubeCoordinate 4 0)
                , (mkCubeCoordinate 3 1, mkCubeCoordinate 4 1)
                , (mkCubeCoordinate 3 2, mkCubeCoordinate 4 1)
                , (mkCubeCoordinate 3 2, mkCubeCoordinate 4 2)
                , (mkCubeCoordinate 3 3, mkCubeCoordinate 4 2)
                ]

    describe "mkTerrainBlock" $ do
        it "assigns one feature to every coordinate in a block" $ do
            mkTerrainBlock Jungle (0, 1) (-1, 0) `shouldBe`
                Map.fromList
                    [ (mkCubeCoordinate 0 (-1), TerrainHex False Jungle [])
                    , (mkCubeCoordinate 0 0, TerrainHex False Jungle [])
                    , (mkCubeCoordinate 1 (-1), TerrainHex False Jungle [])
                    , (mkCubeCoordinate 1 0, TerrainHex False Jungle [])
                    ]

    describe "fillHexOcean" $ do
        it "wraps the bare canned board in two rings of ocean while preserving land" $ do
            fst (fillHexOcean (bareBoard1.board, mkStdGen 0)) `shouldBe` filledBareBoard1.board

    describe "canned boards" $ do
        it "tracks bare board #1 territories in getConnectedSets format" $ do
            getConnectedSets bareBoard1.board `shouldBe` bareBoard1.territories

        it "keeps board #1 invalid without ocean" $ do
            isValidTerrainBoard bareBoard1.board `shouldBe` False

        it "tracks board #2 territories after fillHexOcean" $ do
            getConnectedSets filledBareBoard1.board `shouldBe` filledBareBoard1.territories

        it "makes board #2 valid once ocean is added" $ do
            isValidTerrainBoard filledBareBoard1.board `shouldBe` True

        it "tracks board #3 territories with one 3x3 and one 4x4 block per feature" $ do
            getConnectedSets cannedBoard3.board `shouldBe` cannedBoard3.territories

        it "makes board #3 valid after fillHexOcean" $ do
            isValidTerrainBoard cannedBoard3.board `shouldBe` True

        it "keeps the non-ocean territory graph connected on board #3" $ do
            hasConnectedTerritoryGraph cannedBoard3 `shouldBe` True

        it "keeps same-feature territories separated on board #3" $ do
            sameFeatureTerritoriesSeparated cannedBoard3 `shouldBe` True

        it "tracks board #4 territories with one 3x3, 4x4, and 5x5 block per feature" $ do
            getConnectedSets cannedBoard4.board `shouldBe` cannedBoard4.territories

        it "makes board #4 valid after fillHexOcean" $ do
            isValidTerrainBoard cannedBoard4.board `shouldBe` True

        it "keeps the non-ocean territory graph connected on board #4" $ do
            hasConnectedTerritoryGraph cannedBoard4 `shouldBe` True

        it "keeps same-feature territories separated on board #4" $ do
            sameFeatureTerritoriesSeparated cannedBoard4 `shouldBe` True

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

origin :: CubeCoordinate Int
origin = mkCubeCoordinate 0 0

expectedDistanceOne :: Set.Set (CubeCoordinate Int)
expectedDistanceOne =
    Set.fromList $ origin : map (uncurry mkCubeCoordinate) radiusOneCubeCoordinates

expectedDistanceTwo :: Set.Set (CubeCoordinate Int)
expectedDistanceTwo =
    Set.fromList $
        origin
            : map (uncurry mkCubeCoordinate) (radiusOneCubeCoordinates <> radiusTwoCubeCoordinates)

overlappingSeeds :: Set.Set (CubeCoordinate Int)
overlappingSeeds = Set.fromList [origin, translatedSeed]

translatedSeed :: CubeCoordinate Int
translatedSeed = mkCubeCoordinate 1 0

expectedOverlappingDistanceTwo :: Set.Set (CubeCoordinate Int)
expectedOverlappingDistanceTwo =
    Set.union (translatedRadiusTwo origin) (translatedRadiusTwo translatedSeed)

translatedRadiusTwo :: CubeCoordinate Int -> Set.Set (CubeCoordinate Int)
translatedRadiusTwo center =
    Set.fromList $ center : map (translate center) (radiusOneCubeCoordinates <> radiusTwoCubeCoordinates)

translate :: CubeCoordinate Int -> (Int, Int) -> CubeCoordinate Int
translate center (dq, dr) = mkCubeCoordinate (q + dq) (r + dr)
  where
        (q, r) = toPair center

mkCoordinateBlock :: (Int, Int) -> (Int, Int) -> [CubeCoordinate Int]
mkCoordinateBlock (qMin, qMax) (rMin, rMax) =
    [ mkCubeCoordinate q r
    | q <- [qMin .. qMax]
    , r <- [rMin .. rMax]
    ]

mkTerrainBlock :: Feature -> (Int, Int) -> (Int, Int) -> HexMap
mkTerrainBlock feature qBounds rBounds =
    Map.fromList $ map (, TerrainHex False feature []) (mkCoordinateBlock qBounds rBounds)

mkTestTerrainBoard :: [(Feature, (Int, Int), (Int, Int))] -> HexMap
mkTestTerrainBoard =
    Map.unions . map (\(feature, qBounds, rBounds) -> mkTerrainBlock feature qBounds rBounds)

data CannedBoard = CannedBoard
    { board :: HexMap
    , territories :: [(Feature, [HexMap])]
    }

type TerritorySpec = (Feature, (Int, Int), (Int, Int))

findBoundaryContacts :: [CubeCoordinate Int] -> [CubeCoordinate Int] -> [(CubeCoordinate Int, CubeCoordinate Int)]
findBoundaryContacts leftCoords rightCoords =
    [ (leftCoord, rightCoord)
    | leftCoord <- leftCoords
    , rightCoord <- rightCoords
    , isUnitCubeDist leftCoord rightCoord
    ]

leftBlockCoords :: [CubeCoordinate Int]
leftBlockCoords = mkCoordinateBlock (0, 1) (0, 1)

rightBlockCoords :: [CubeCoordinate Int]
rightBlockCoords = mkCoordinateBlock (2, 3) (0, 1)

leftBlockCoords3x3 :: [CubeCoordinate Int]
leftBlockCoords3x3 = mkCoordinateBlock (0, 2) (0, 2)

rightBlockCoords3x3 :: [CubeCoordinate Int]
rightBlockCoords3x3 = mkCoordinateBlock (3, 5) (0, 2)

leftBlockCoords4x4 :: [CubeCoordinate Int]
leftBlockCoords4x4 = mkCoordinateBlock (0, 3) (0, 3)

rightBlockCoords3x3Shifted :: [CubeCoordinate Int]
rightBlockCoords3x3Shifted = mkCoordinateBlock (4, 6) (0, 2)

testTerrainBoard :: HexMap
testTerrainBoard =
    mkTestTerrainBoard
        [ (Jungle, (0, 1), (0, 1))
        , (Beach, (2, 3), (0, 1))
        ]

testTerrainBoard3x3 :: HexMap
testTerrainBoard3x3 =
    mkTestTerrainBoard
        [ (Jungle, (0, 2), (0, 2))
        , (Beach, (3, 5), (0, 2))
        ]

testTerrainBoard4x4And3x3 :: HexMap
testTerrainBoard4x4And3x3 =
    mkTestTerrainBoard
        [ (Jungle, (0, 3), (0, 3))
        , (Beach, (4, 6), (0, 2))
        ]

bareBoard1 :: CannedBoard
bareBoard1 = mkBareCannedBoard bareBoard1Specs

filledBareBoard1 :: CannedBoard
filledBareBoard1 = fillOceanCannedBoard bareBoard1

cannedBoard3 :: CannedBoard
cannedBoard3 = fillOceanCannedBoard (mkBareCannedBoard board3Specs)

cannedBoard4 :: CannedBoard
cannedBoard4 = fillOceanCannedBoard (mkBareCannedBoard board4Specs)

bareBoard1Specs :: [TerritorySpec]
bareBoard1Specs =
    [ (Lagoon, (0, 1), (0, 1))
    , (River, (4, 5), (0, 1))
    , (Mountain, (8, 9), (0, 1))
    , (Jungle, (12, 13), (0, 1))
    , (Beach, (16, 17), (0, 1))
    , (Meadow, (20, 21), (0, 1))
    ]

board3Specs :: [TerritorySpec]
board3Specs =
        [ (Lagoon, (0, 3), (0, 3))
        , (River, (4, 7), (0, 3))
        , (Mountain, (8, 11), (0, 3))
        , (Jungle, (12, 15), (0, 3))
        , (Beach, (16, 19), (0, 3))
        , (Meadow, (20, 23), (0, 3))
        , (Lagoon, (24, 26), (0, 2))
        , (River, (27, 29), (0, 2))
        , (Mountain, (30, 32), (0, 2))
        , (Jungle, (33, 35), (0, 2))
        , (Beach, (36, 38), (0, 2))
        , (Meadow, (39, 41), (0, 2))
        ]

board4Specs :: [TerritorySpec]
board4Specs =
        [ (Lagoon, (0, 4), (0, 4))
        , (River, (5, 9), (0, 4))
        , (Mountain, (10, 14), (0, 4))
        , (Jungle, (15, 19), (0, 4))
        , (Beach, (20, 24), (0, 4))
        , (Meadow, (25, 29), (0, 4))
        , (Lagoon, (30, 33), (0, 3))
        , (River, (34, 37), (0, 3))
        , (Mountain, (38, 41), (0, 3))
        , (Jungle, (42, 45), (0, 3))
        , (Beach, (46, 49), (0, 3))
        , (Meadow, (50, 53), (0, 3))
        , (Lagoon, (54, 56), (0, 2))
        , (River, (57, 59), (0, 2))
        , (Mountain, (60, 62), (0, 2))
        , (Jungle, (63, 65), (0, 2))
        , (Beach, (66, 68), (0, 2))
        , (Meadow, (69, 71), (0, 2))
        ]

mkBareCannedBoard :: [TerritorySpec] -> CannedBoard
mkBareCannedBoard specs =
    CannedBoard
        { board = mkTestTerrainBoard specs
        , territories = territoriesFromSpecs specs
        }

fillOceanCannedBoard :: CannedBoard -> CannedBoard
fillOceanCannedBoard canned =
    CannedBoard
        { board = oceanBoard
        , territories = injectOceanTerritories oceanBoard canned.territories
        }
  where
    oceanBoard = fst (fillHexOcean (canned.board, mkStdGen 0))

territoriesFromSpecs :: [TerritorySpec] -> [(Feature, [HexMap])]
territoriesFromSpecs specs = map territoriesForFeature orderedFeatures
  where
    orderedFeatures = [Ocean, Lagoon, River, Mountain, Jungle, Beach, Meadow]
    territoriesForFeature Ocean = (Ocean, [])
    territoriesForFeature feature =
        ( feature
        , sortTerritoriesDescending
            [ mkTerrainBlock territoryFeature qBounds rBounds
            | (territoryFeature, qBounds, rBounds) <- specs
            , territoryFeature == feature
            ]
        )

injectOceanTerritories :: HexMap -> [(Feature, [HexMap])] -> [(Feature, [HexMap])]
injectOceanTerritories fullBoard = map injectOcean
  where
    connectedSets = getConnectedSets fullBoard
    injectOcean (Ocean, _) = (Ocean, lookupFeatureTerritories Ocean connectedSets)
    injectOcean featureTerritories = featureTerritories

lookupFeatureTerritories :: Feature -> [(Feature, [HexMap])] -> [HexMap]
lookupFeatureTerritories feature connectedSets =
        maybe [] (sortTerritoriesDescending . snd) matchingFeature
    where
        matchingFeature = find ((== feature) . fst) connectedSets

sortTerritoriesDescending :: [HexMap] -> [HexMap]
sortTerritoriesDescending = sortBy (flip compare `on` Map.size)

hasConnectedTerritoryGraph :: CannedBoard -> Bool
hasConnectedTerritoryGraph canned
    | null indexedTerritories = True
    | otherwise = length visited == length indexedTerritories
  where
    indexedTerritories = zip [0 :: Int ..] (concatMap snd nonOceanGroups)
    nonOceanGroups = filter ((/= Ocean) . fst) canned.territories
    visited = explore [] [0]

    explore seen [] = seen
    explore seen (indexToVisit:rest)
        | indexToVisit `elem` seen = explore seen rest
        | otherwise = explore (indexToVisit : seen) (neighbors indexToVisit <> rest)

    neighbors territoryIndex =
        [ candidateIndex
        | (candidateIndex, candidateTerritory) <- indexedTerritories
        , candidateIndex /= territoryIndex
        , territoriesTouch (territoryAt territoryIndex) candidateTerritory
        ]

    territoryAt territoryIndex =
        snd $ fromMaybe (error "missing territory index") $ find ((== territoryIndex) . fst) indexedTerritories

sameFeatureTerritoriesSeparated :: CannedBoard -> Bool
sameFeatureTerritoriesSeparated canned = all featureSeparated nonOceanGroups
  where
    nonOceanGroups = filter ((/= Ocean) . fst) canned.territories
    featureSeparated (_, territoriesForFeature) =
        and
            [ not (territoriesTouch leftTerritory rightTerritory)
            | (index, leftTerritory) <- zip [0 :: Int ..] territoriesForFeature
            , rightTerritory <- drop (index + 1) territoriesForFeature
            ]

territoriesTouch :: HexMap -> HexMap -> Bool
territoriesTouch leftTerritory rightTerritory =
    any (uncurry isUnitCubeDist)
        [ (leftCoord, rightCoord)
        | leftCoord <- Map.keys leftTerritory
        , rightCoord <- Map.keys rightTerritory
        ]
