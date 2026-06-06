module Game.RealEstate.VoxelVerse.Types
    ( InteractionState (..)
    , ProjectionState (..)
    )
where

import Data.Aeson (FromJSON, ToJSON)

-- | Placeholder VoxelVerse interaction state for Real Estate.
data InteractionState = InteractionState
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Placeholder VoxelVerse projection state for Real Estate.
data ProjectionState = ProjectionState
    deriving (Show, Eq, Generic, FromJSON, ToJSON)
