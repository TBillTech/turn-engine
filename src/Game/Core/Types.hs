module Game.Core.Types
    ( module Game.Core.Primitives
    , GameState (..)
    )
where

import Data.Aeson (FromJSON, ToJSON)

import Game.Core.Primitives
import qualified Game.ArtOfWar.Types as ArtOfWar
import qualified Game.CursedTreasure.Types as CursedTreasure
import qualified Game.FogOfBattle.Types as FogOfBattle
import qualified Game.RealEstate.Types as RealEstate

-- | Represents the overall state of a game in progress.
--
-- This is a tagged union over the concrete ruleset states.
data GameState
    = CursedTreasureGame CursedTreasure.GameState
    | FogOfBattleGame FogOfBattle.GameState
    | ArtOfWarGame ArtOfWar.GameState
    | RealEstateGame RealEstate.GameState
    deriving (Show, Eq, Generic, FromJSON, ToJSON)
