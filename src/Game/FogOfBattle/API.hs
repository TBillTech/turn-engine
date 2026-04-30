module Game.FogOfBattle.API
    ( getGameSetupPlayers
    , createNewGame
    , enumerateActivePlayerOptions
    , makeMove
    , heuristicHint
    )
where

import Game.FogOfBattle.Types

getGameSetupPlayers :: [PlayerDescription]
getGameSetupPlayers = []

createNewGame :: [PlayerDescription] -> Int -> (GameState, [CensoredGameState])
createNewGame players _ = (gameState, map (mkCensoredGameState gameState) players)
    where
        gameState = GameState
            { players = players
            , turn = 0
            , gameOver = True
            }

enumerateActivePlayerOptions :: GameState -> [PlayerMove]
enumerateActivePlayerOptions _ = []

heuristicHint :: Int -> GameState -> [PlayerMove] -> [(Int, PlayerMove)]
heuristicHint _ _ = map (0,)

makeMove :: GameState -> PlayerMove -> (GameState, [CensoredGameState])
makeMove gameState _ = (gameState, map (mkCensoredGameState gameState) gameState.players)

mkCensoredGameState :: GameState -> PlayerDescription -> CensoredGameState
mkCensoredGameState gameState _ = CensoredGameState gameState