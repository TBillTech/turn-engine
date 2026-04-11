module Game.Logic where

import Game.Types

-- | Advance the game to the next turn.
nextTurn :: GameState -> GameState
nextTurn gs = gs { turn = gs.turn + 1 }
