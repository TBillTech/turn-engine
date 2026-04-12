module Game.RealEstate.Types where

import Data.Aeson (FromJSON, ToJSON)

-- | Placeholder ruleset state for Real Estate.
-- Expand with actual ruleset-specific state as the game is implemented.
data GameState = GameState
    { players :: [Text]
    , turn :: Int
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)
