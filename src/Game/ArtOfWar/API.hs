module Game.ArtOfWar.API
    ( getGameSetupPlayers
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

import Game.Core.Primitives (PlayerId, SeedStream, mkPlayerId, mkSeedStream)
import Game.ArtOfWar.Types

getGameSetupPlayers :: [PlayerDescription]
getGameSetupPlayers = []

createNewGame :: [PlayerDescription] -> Int -> (GameState, [CensoredGameState])
createNewGame players randomSeed = (gameState, map (mkCensoredGameState gameState) players)
    where
        firstPlayer = fromMaybe (error "Art Of War requires at least one player") (mkPlayerId 1)
        gameState = GameState
            { players = players
            , turn = 0
            , activePlayer = firstPlayer
            , latestMessage = "Art Of War setup complete"
            , gameOver = True
            , seed = mkSeedStream randomSeed 1
            }

getTurn :: GameState -> Int
getTurn gameState = gameState.turn

getActivePlayer :: GameState -> PlayerId
getActivePlayer gameState = gameState.activePlayer

getLatestMessage :: GameState -> Text
getLatestMessage gameState = gameState.latestMessage

getGameOver :: GameState -> Bool
getGameOver gameState = gameState.gameOver

getSeed :: GameState -> SeedStream
getSeed gameState = gameState.seed

enumerateActivePlayerOptions :: GameState -> [PlayerMove]
enumerateActivePlayerOptions _ = []

heuristicHint :: Int -> GameState -> [PlayerMove] -> [(Int, PlayerMove)]
heuristicHint _ _ = map (0,)

makeMove :: GameState -> PlayerMove -> (GameState, [CensoredGameState])
makeMove gameState _ = (gameState, map (mkCensoredGameState gameState) gameState.players)

summary :: GameState -> Text
summary _ = "undefined"

mkCensoredGameState :: GameState -> PlayerDescription -> CensoredGameState
mkCensoredGameState gameState _ = CensoredGameState gameState