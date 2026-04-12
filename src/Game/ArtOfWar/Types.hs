module Game.ArtOfWar.Types where

import Data.Aeson (FromJSON, ToJSON)

-- | Placeholder ruleset state for Art Of War.
-- Expand with actual ruleset-specific state as the game is implemented.
data GameState = GameState
    { players :: [Text]
    , turn :: Int
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)
