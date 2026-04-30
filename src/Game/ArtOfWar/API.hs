module Game.ArtOfWar.API
    ( getGameSetupPlayers
    , createNewGame
    , enumerateActivePlayerOptions
    , makeMove
    )
where

import Game.ArtOfWar.Types

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

makeMove :: GameState -> PlayerMove -> (GameState, [CensoredGameState])
makeMove gameState _ = (gameState, map (mkCensoredGameState gameState) gameState.players)

mkCensoredGameState :: GameState -> PlayerDescription -> CensoredGameState
mkCensoredGameState gameState _ = CensoredGameState gameState