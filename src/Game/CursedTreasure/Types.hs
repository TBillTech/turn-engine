module Game.CursedTreasure.Types where

import Data.Aeson (FromJSON, ToJSON)

import Game.Core.Primitives (TokenSpace)

data Feature
    = Ocean
    | PalmTree
    | Statue
    | River
    | Mountain
    | Jungle
    | Beach
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data ClueCard
    = WithinStepsOfLargest Int Feature
    | WithinStepsOfSmallest Int Feature
    | NotWithinStepsOfLargest Int Feature
    | NotWithinStepsOfSmallest Int Feature
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data PlayerState = PlayerState
    {
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data TerrainHex = TerrainHex
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

type ClueCards = TokenSpace Text Int ClueCard

-- | State for a Cursed Treasure game.
data GameState = GameState
    { players :: [PlayerState]
    , turn :: Int
    , terrainBoard :: TokenSpace Text (Int, Int) TerrainHex
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)
