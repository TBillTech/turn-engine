module Game.Core.Types
    ( module Game.Core.Primitives
    , GameState (..)
    , PlayerDescription (..)
    , PlayerMove (..)
    , CensoredGameState (..)
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

data PlayerDescription
    = CursedTreasurePlayerDescription CursedTreasure.PlayerDescription
    | FogOfBattlePlayerDescription FogOfBattle.PlayerDescription
    | ArtOfWarPlayerDescription ArtOfWar.PlayerDescription
    | RealEstatePlayerDescription RealEstate.PlayerDescription
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data PlayerMove
    = CursedTreasurePlayerMove CursedTreasure.PlayerMove
    | FogOfBattlePlayerMove FogOfBattle.PlayerMove
    | ArtOfWarPlayerMove ArtOfWar.PlayerMove
    | RealEstatePlayerMove RealEstate.PlayerMove
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data CensoredGameState
    = CursedTreasureCensoredGameState CursedTreasure.CensoredGameState
    | FogOfBattleCensoredGameState FogOfBattle.CensoredGameState
    | ArtOfWarCensoredGameState ArtOfWar.CensoredGameState
    | RealEstateCensoredGameState RealEstate.CensoredGameState
    deriving (Show, Eq, Generic, ToJSON)
