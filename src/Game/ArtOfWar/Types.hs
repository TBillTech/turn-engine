module Game.ArtOfWar.Types
    ( PlayerDescription(..)
    , PlayerMove(..)
    , CensoredGameState(..)
    , GameState(..)
    , validateSetupPlayers
    , validateGameState
    )
where

import Data.Aeson (FromJSON (..), ToJSON, (.:), (.:?), (.!=), withObject)
import Game.Core.Primitives (PlayerId, SeedStream, mkPlayerId, mkSeedStream)

data PlayerDescription = PlayerDescription
    { playerName :: Text
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data PlayerMove = NoOpMove
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype CensoredGameState = CensoredGameState GameState
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Placeholder ruleset state for Art Of War.
-- Expand with actual ruleset-specific state as the game is implemented.
data GameState = GameState
    { players :: [PlayerDescription]
    , turn :: Int
    , activePlayer :: PlayerId
    , latestMessage :: Text
    , gameOver :: Bool
    , seed :: SeedStream
    }
    deriving (Show, Eq, Generic, ToJSON)

instance FromJSON GameState where
    parseJSON = withObject "GameState" $ \obj -> do
        players <- obj .: "players"
        turn <- obj .: "turn"
        gameOver <- obj .: "gameOver"
        activePlayer <- obj .:? "activePlayer" .!= defaultPlayerId
        latestMessage <- obj .:? "latestMessage" .!= ""
        seed <- obj .:? "seed" .!= mkSeedStream 0 0
        pure GameState
            { players = players
            , turn = turn
            , activePlayer = activePlayer
            , latestMessage = latestMessage
            , gameOver = gameOver
            , seed = seed
            }
      where
        defaultPlayerId = fromMaybe (error "PlayerId 1 must be valid") (mkPlayerId 1)

validateSetupPlayers :: [PlayerDescription] -> Either Text [PlayerDescription]
validateSetupPlayers players = do
    when (null players) $ Left "Art Of War requires at least one player"
    pure players

validateGameState :: GameState -> Either Text GameState
validateGameState gameState = validateSetupPlayers gameState.players $> gameState
