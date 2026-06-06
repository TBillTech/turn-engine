module Game.CursedTreasure.VoxelVerse.Types
    ( InteractionState (..)
    , ProjectionState (..)
    )
where

import Data.Aeson (FromJSON, ToJSON)

-- | Placeholder VoxelVerse interaction state for Cursed Treasure.
data InteractionState = InteractionState {
    
} deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Placeholder VoxelVerse projection state for Cursed Treasure.
data ProjectionState = ProjectionState {

} deriving (Show, Eq, Generic, FromJSON, ToJSON)
