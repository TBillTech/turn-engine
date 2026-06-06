module Game.ArtOfWar.VoxelVerse.Types
    ( InteractionState (..)
    , ProjectionState (..)
    )
where

import Data.Aeson (FromJSON, ToJSON)

-- | Placeholder VoxelVerse interaction state for Art Of War.
data InteractionState = InteractionState
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Placeholder VoxelVerse projection state for Art Of War.
data ProjectionState = ProjectionState
    deriving (Show, Eq, Generic, FromJSON, ToJSON)
