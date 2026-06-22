module Game.CursedTreasure.VoxelVerse.Types
    ( InteractionState (..)
    , ProjectionState (..)
    )
where

import Game.CursedTreasure.Types
import Game.Core.Primitives

import Data.Aeson (FromJSON, ToJSON)

-- | Placeholder VoxelVerse interaction state for Cursed Treasure.
data InteractionState = InteractionState {
    currentToolChoices :: ToolApplication,
    enumeratedPlayerMoves :: [PlayerMove]
} deriving (Show, Eq, Generic)

-- | Placeholder VoxelVerse projection state for Cursed Treasure.
data ProjectionState = ProjectionState {

} deriving (Show, Eq, Generic)

