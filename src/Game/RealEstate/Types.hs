module Game.RealEstate.Types where

import Data.Aeson (FromJSON, ToJSON)

data PlayerDescription = PlayerDescription
    { playerName :: Text
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data PlayerMove = NoOpMove
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype CensoredGameState = CensoredGameState GameState
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Placeholder ruleset state for Real Estate.
-- Expand with actual ruleset-specific state as the game is implemented.
data GameState = GameState
    { players :: [PlayerDescription]
    , turn :: Int
    , gameOver :: Bool
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)
