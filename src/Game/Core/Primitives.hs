module Game.Core.Primitives where

import Data.Aeson (FromJSON, ToJSON)

-- | Defines how positions/tokens are considered adjacent.
data AdjacencyPolicy
    = IndexAdjacency
    | HexAdjacency
    | NonAdjacency
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | TokenSpace is annotated by an adjacency policy and contains tokens keyed by 'k'.
type TokenSpace a k t = (a, Map k t)
