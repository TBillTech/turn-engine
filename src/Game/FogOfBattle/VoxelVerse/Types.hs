module Game.FogOfBattle.VoxelVerse.Types
    ( InteractionState (..)
    , ProjectionState (..)
    )
where

import Data.Aeson (FromJSON, ToJSON)

-- | Placeholder VoxelVerse interaction state for Fog Of Battle.
data InteractionState = InteractionState
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Placeholder VoxelVerse projection state for Fog Of Battle.
data ProjectionState = ProjectionState
    deriving (Show, Eq, Generic, FromJSON, ToJSON)
