module Game.Logic where

import qualified Game.ArtOfWar.Types as ArtOfWar
import qualified Game.CursedTreasure.Types as CursedTreasure
import qualified Game.Core.Types as Core
import qualified Game.FogOfBattle.Types as FogOfBattle
import qualified Game.RealEstate.Types as RealEstate

-- | Advance the game to the next turn.
nextTurn :: Core.GameState -> Core.GameState
nextTurn = \case
    Core.CursedTreasureGame gs ->
        Core.CursedTreasureGame (gs { CursedTreasure.turn = gs.turn + 1 })
    Core.FogOfBattleGame gs ->
        Core.FogOfBattleGame (gs { FogOfBattle.turn = gs.turn + 1 })
    Core.ArtOfWarGame gs ->
        Core.ArtOfWarGame (gs { ArtOfWar.turn = gs.turn + 1 })
    Core.RealEstateGame gs ->
        Core.RealEstateGame (gs { RealEstate.turn = gs.turn + 1 })
