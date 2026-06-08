module Game.Core.API
    ( getGameSetupPlayers
    , validateSetupPlayers
    , validateGameState
    , createNewGame
    , getTurn
    , getActivePlayer
    , getLatestMessage
    , getGameOver
    , getSeed
    , enumerateActivePlayerOptions
    , makeMove
    , heuristicHint
    , summary
    )
where

import qualified Game.ArtOfWar.API as ArtOfWar
import qualified Game.ArtOfWar.Types as ArtOfWar
import qualified Game.Core.Types as Core
import qualified Game.CursedTreasure.API as CursedTreasure
import qualified Game.CursedTreasure.Types as CursedTreasure
import qualified Game.FogOfBattle.API as FogOfBattle
import qualified Game.FogOfBattle.Types as FogOfBattle
import qualified Game.RealEstate.API as RealEstate
import qualified Game.RealEstate.Types as RealEstate

getGameSetupPlayers :: [(Text, [Core.PlayerDescription])]
getGameSetupPlayers =
    [ ("Cursed Treasure", map Core.CursedTreasurePlayerDescription CursedTreasure.getGameSetupPlayers)
    , ("Fog Of Battle", map Core.FogOfBattlePlayerDescription FogOfBattle.getGameSetupPlayers)
    , ("Art Of War", map Core.ArtOfWarPlayerDescription ArtOfWar.getGameSetupPlayers)
    , ("Real Estate", map Core.RealEstatePlayerDescription RealEstate.getGameSetupPlayers)
    ]

createNewGame :: [Core.PlayerDescription] -> Int -> Either Text (Core.GameState, [Core.CensoredGameState])
createNewGame players randomSeed =
    validateSetupPlayers players >>
        case players of
            [] -> Left "PlayerDescription list must be non-empty and all match the same ruleset."
            (Core.CursedTreasurePlayerDescription _ : _) ->
                let (gameState, censoredStates) =
                        CursedTreasure.createNewGame (expectPlayers unwrapCursedTreasure players) randomSeed
                 in Right
                        ( Core.CursedTreasureGame gameState
                        , map Core.CursedTreasureCensoredGameState censoredStates
                        )
            (Core.FogOfBattlePlayerDescription _ : _) ->
                let (gameState, censoredStates) =
                        FogOfBattle.createNewGame (expectPlayers unwrapFogOfBattle players) randomSeed
                 in Right
                        ( Core.FogOfBattleGame gameState
                        , map Core.FogOfBattleCensoredGameState censoredStates
                        )
            (Core.ArtOfWarPlayerDescription _ : _) ->
                let (gameState, censoredStates) =
                        ArtOfWar.createNewGame (expectPlayers unwrapArtOfWar players) randomSeed
                 in Right
                        ( Core.ArtOfWarGame gameState
                        , map Core.ArtOfWarCensoredGameState censoredStates
                        )
            (Core.RealEstatePlayerDescription _ : _) ->
                let (gameState, censoredStates) =
                        RealEstate.createNewGame (expectPlayers unwrapRealEstate players) randomSeed
                 in Right
                        ( Core.RealEstateGame gameState
                        , map Core.RealEstateCensoredGameState censoredStates
                        )

validateSetupPlayers :: [Core.PlayerDescription] -> Either Text [Core.PlayerDescription]
validateSetupPlayers players =
    case players of
        [] -> Left "PlayerDescription list must be non-empty and all match the same ruleset."
        (Core.CursedTreasurePlayerDescription _ : _)
            | all isCursedTreasurePlayer players ->
                map Core.CursedTreasurePlayerDescription <$> CursedTreasure.validateSetupPlayers (expectPlayers unwrapCursedTreasure players)
            | otherwise -> Left "PlayerDescription values must all match the same ruleset."
        (Core.FogOfBattlePlayerDescription _ : _)
            | all isFogOfBattlePlayer players ->
                map Core.FogOfBattlePlayerDescription <$> FogOfBattle.validateSetupPlayers (expectPlayers unwrapFogOfBattle players)
            | otherwise -> Left "PlayerDescription values must all match the same ruleset."
        (Core.ArtOfWarPlayerDescription _ : _)
            | all isArtOfWarPlayer players ->
                map Core.ArtOfWarPlayerDescription <$> ArtOfWar.validateSetupPlayers (expectPlayers unwrapArtOfWar players)
            | otherwise -> Left "PlayerDescription values must all match the same ruleset."
        (Core.RealEstatePlayerDescription _ : _)
            | all isRealEstatePlayer players ->
                map Core.RealEstatePlayerDescription <$> RealEstate.validateSetupPlayers (expectPlayers unwrapRealEstate players)
            | otherwise -> Left "PlayerDescription values must all match the same ruleset."

validateGameState :: Core.GameState -> Either Text Core.GameState
validateGameState = \case
    Core.CursedTreasureGame gameState -> Core.CursedTreasureGame <$> CursedTreasure.validateGameState gameState
    Core.FogOfBattleGame gameState -> Core.FogOfBattleGame <$> FogOfBattle.validateGameState gameState
    Core.ArtOfWarGame gameState -> Core.ArtOfWarGame <$> ArtOfWar.validateGameState gameState
    Core.RealEstateGame gameState -> Core.RealEstateGame <$> RealEstate.validateGameState gameState

getTurn :: Core.GameState -> Int
getTurn = \case
    Core.CursedTreasureGame gameState -> CursedTreasure.getTurn gameState
    Core.FogOfBattleGame gameState -> FogOfBattle.getTurn gameState
    Core.ArtOfWarGame gameState -> ArtOfWar.getTurn gameState
    Core.RealEstateGame gameState -> RealEstate.getTurn gameState

getActivePlayer :: Core.GameState -> Core.PlayerId
getActivePlayer = \case
    Core.CursedTreasureGame gameState -> CursedTreasure.getActivePlayer gameState
    Core.FogOfBattleGame gameState -> FogOfBattle.getActivePlayer gameState
    Core.ArtOfWarGame gameState -> ArtOfWar.getActivePlayer gameState
    Core.RealEstateGame gameState -> RealEstate.getActivePlayer gameState

getLatestMessage :: Core.GameState -> Text
getLatestMessage = \case
    Core.CursedTreasureGame gameState -> CursedTreasure.getLatestMessage gameState
    Core.FogOfBattleGame gameState -> FogOfBattle.getLatestMessage gameState
    Core.ArtOfWarGame gameState -> ArtOfWar.getLatestMessage gameState
    Core.RealEstateGame gameState -> RealEstate.getLatestMessage gameState

getGameOver :: Core.GameState -> Bool
getGameOver = \case
    Core.CursedTreasureGame gameState -> CursedTreasure.getGameOver gameState
    Core.FogOfBattleGame gameState -> FogOfBattle.getGameOver gameState
    Core.ArtOfWarGame gameState -> ArtOfWar.getGameOver gameState
    Core.RealEstateGame gameState -> RealEstate.getGameOver gameState

getSeed :: Core.GameState -> Core.SeedStream
getSeed = \case
    Core.CursedTreasureGame gameState -> CursedTreasure.getSeed gameState
    Core.FogOfBattleGame gameState -> FogOfBattle.getSeed gameState
    Core.ArtOfWarGame gameState -> ArtOfWar.getSeed gameState
    Core.RealEstateGame gameState -> RealEstate.getSeed gameState

enumerateActivePlayerOptions :: Core.GameState -> [Core.PlayerMove]
enumerateActivePlayerOptions = \case
    Core.CursedTreasureGame gameState ->
        map Core.CursedTreasurePlayerMove (CursedTreasure.enumerateActivePlayerOptions gameState)
    Core.FogOfBattleGame gameState ->
        map Core.FogOfBattlePlayerMove (FogOfBattle.enumerateActivePlayerOptions gameState)
    Core.ArtOfWarGame gameState ->
        map Core.ArtOfWarPlayerMove (ArtOfWar.enumerateActivePlayerOptions gameState)
    Core.RealEstateGame gameState ->
        map Core.RealEstatePlayerMove (RealEstate.enumerateActivePlayerOptions gameState)

heuristicHint :: Int -> Core.GameState -> [Core.PlayerMove] -> [(Int, Core.PlayerMove)]
heuristicHint level gameState playerMoves =
    case (gameState, playerMoves) of
        (Core.CursedTreasureGame rulesetState, moves)
            | Just rulesetMoves <- traverse unwrapCursedTreasureMove moves ->
                map (fmap Core.CursedTreasurePlayerMove) (CursedTreasure.heuristicHint level rulesetState rulesetMoves)
        (Core.FogOfBattleGame rulesetState, moves)
            | Just rulesetMoves <- traverse unwrapFogOfBattleMove moves ->
                map (fmap Core.FogOfBattlePlayerMove) (FogOfBattle.heuristicHint level rulesetState rulesetMoves)
        (Core.ArtOfWarGame rulesetState, moves)
            | Just rulesetMoves <- traverse unwrapArtOfWarMove moves ->
                map (fmap Core.ArtOfWarPlayerMove) (ArtOfWar.heuristicHint level rulesetState rulesetMoves)
        (Core.RealEstateGame rulesetState, moves)
            | Just rulesetMoves <- traverse unwrapRealEstateMove moves ->
                map (fmap Core.RealEstatePlayerMove) (RealEstate.heuristicHint level rulesetState rulesetMoves)
        _ -> error "PlayerMove list does not match GameState ruleset."

summary :: Core.GameState -> Text
summary = \case
    Core.CursedTreasureGame gameState -> CursedTreasure.summary gameState
    Core.FogOfBattleGame gameState -> FogOfBattle.summary gameState
    Core.ArtOfWarGame gameState -> ArtOfWar.summary gameState
    Core.RealEstateGame gameState -> RealEstate.summary gameState

makeMove :: Core.GameState -> Core.PlayerMove -> (Core.GameState, [Core.CensoredGameState])
makeMove gameState playerMove =
    case (gameState, playerMove) of
        (Core.CursedTreasureGame rulesetState, Core.CursedTreasurePlayerMove move) ->
            let (nextState, censoredStates) = CursedTreasure.makeMove rulesetState move
             in ( Core.CursedTreasureGame nextState
                , map Core.CursedTreasureCensoredGameState censoredStates
                )
        (Core.FogOfBattleGame rulesetState, Core.FogOfBattlePlayerMove move) ->
            let (nextState, censoredStates) = FogOfBattle.makeMove rulesetState move
             in ( Core.FogOfBattleGame nextState
                , map Core.FogOfBattleCensoredGameState censoredStates
                )
        (Core.ArtOfWarGame rulesetState, Core.ArtOfWarPlayerMove move) ->
            let (nextState, censoredStates) = ArtOfWar.makeMove rulesetState move
             in ( Core.ArtOfWarGame nextState
                , map Core.ArtOfWarCensoredGameState censoredStates
                )
        (Core.RealEstateGame rulesetState, Core.RealEstatePlayerMove move) ->
            let (nextState, censoredStates) = RealEstate.makeMove rulesetState move
             in ( Core.RealEstateGame nextState
                , map Core.RealEstateCensoredGameState censoredStates
                )
        _ -> error "PlayerMove does not match GameState ruleset."

expectPlayers :: (Core.PlayerDescription -> Maybe playerDescription) -> [Core.PlayerDescription] -> [playerDescription]
expectPlayers unwrap players
    | length unwrappedPlayers == length players = unwrappedPlayers
    | otherwise = error "PlayerDescription values must all match the selected ruleset."
    where
        unwrappedPlayers = mapMaybe unwrap players

isCursedTreasurePlayer :: Core.PlayerDescription -> Bool
isCursedTreasurePlayer = \case
    Core.CursedTreasurePlayerDescription _ -> True
    _ -> False

isFogOfBattlePlayer :: Core.PlayerDescription -> Bool
isFogOfBattlePlayer = \case
    Core.FogOfBattlePlayerDescription _ -> True
    _ -> False

isArtOfWarPlayer :: Core.PlayerDescription -> Bool
isArtOfWarPlayer = \case
    Core.ArtOfWarPlayerDescription _ -> True
    _ -> False

isRealEstatePlayer :: Core.PlayerDescription -> Bool
isRealEstatePlayer = \case
    Core.RealEstatePlayerDescription _ -> True
    _ -> False

unwrapCursedTreasure :: Core.PlayerDescription -> Maybe CursedTreasure.PlayerDescription
unwrapCursedTreasure = \case
    Core.CursedTreasurePlayerDescription player -> Just player
    _ -> Nothing

unwrapFogOfBattle :: Core.PlayerDescription -> Maybe FogOfBattle.PlayerDescription
unwrapFogOfBattle = \case
    Core.FogOfBattlePlayerDescription player -> Just player
    _ -> Nothing

unwrapArtOfWar :: Core.PlayerDescription -> Maybe ArtOfWar.PlayerDescription
unwrapArtOfWar = \case
    Core.ArtOfWarPlayerDescription player -> Just player
    _ -> Nothing

unwrapRealEstate :: Core.PlayerDescription -> Maybe RealEstate.PlayerDescription
unwrapRealEstate = \case
    Core.RealEstatePlayerDescription player -> Just player
    _ -> Nothing

unwrapCursedTreasureMove :: Core.PlayerMove -> Maybe CursedTreasure.PlayerMove
unwrapCursedTreasureMove = \case
    Core.CursedTreasurePlayerMove playerMove -> Just playerMove
    _ -> Nothing

unwrapFogOfBattleMove :: Core.PlayerMove -> Maybe FogOfBattle.PlayerMove
unwrapFogOfBattleMove = \case
    Core.FogOfBattlePlayerMove playerMove -> Just playerMove
    _ -> Nothing

unwrapArtOfWarMove :: Core.PlayerMove -> Maybe ArtOfWar.PlayerMove
unwrapArtOfWarMove = \case
    Core.ArtOfWarPlayerMove playerMove -> Just playerMove
    _ -> Nothing

unwrapRealEstateMove :: Core.PlayerMove -> Maybe RealEstate.PlayerMove
unwrapRealEstateMove = \case
    Core.RealEstatePlayerMove playerMove -> Just playerMove
    _ -> Nothing