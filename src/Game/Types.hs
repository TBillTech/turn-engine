module Game.Types where

import Data.Aeson (FromJSON, ToJSON)

-- | Represents the overall state of a game in progress.
data GameState = GameState
    { players :: [Text]
    , turn    :: Int
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)
