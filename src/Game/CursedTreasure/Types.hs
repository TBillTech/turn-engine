{-# LANGUAGE DerivingStrategies #-}
module Game.CursedTreasure.Types
    ( PlayerColor(..)
    , ClueColor(..)
    , OtherColor(..)
    , PlayerId
    , mkPlayerId
    , unPlayerId
    , allPlayerIds
    , Feature(..)
    , allFeatures
    , ClueObject(..)
    , ClueCard(..)
    , TreasureCard(..)
    , PlayerDescription(..)
    , Score(..)
    , PlayerState(..)
    , TerrainToken(..)
    , TerrainHex (..)
    , CubeCoordinate
    , ClueBoard
    , HexBoard
    , Deck
    , TreasureBoard
    , GameState(..)
    , ToGameState(..)
    , CensoredGameState
    , mkCensoredGameState
    , censorRaisingTreasure
    , RaisingTreasureState(..)
    , PlayerMove(..)
    , GameMode(..)
    , mkPlayerColor
    , allPlayerColors
    , mkClueColor
    , allClueColors
    )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withScientific)

import Game.Core.Primitives (CubeCoordinateTokens, CubeCoordinate,
    HourHand, GameColor (..), ToGameColor(..), allGameColors)

newtype PlayerColor = PlayerColor GameColor
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)

instance ToGameColor PlayerColor where
    toGameColor :: PlayerColor -> GameColor
    toGameColor (PlayerColor c) = c

newtype ClueColor = ClueColor GameColor
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)

instance ToGameColor ClueColor where
    toGameColor :: ClueColor -> GameColor
    toGameColor (ClueColor c) = c

newtype OtherColor = OtherColor GameColor
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)

newtype PlayerId = PlayerId Int
    deriving stock (Show, Eq, Ord, Generic)

mkPlayerId :: Int -> Maybe PlayerId
mkPlayerId playerId
    | playerId `elem` [1 .. 4] = Just (PlayerId playerId)
    | otherwise = Nothing

unPlayerId :: PlayerId -> Int
unPlayerId (PlayerId playerId) = playerId

allPlayerIds :: [PlayerId]
allPlayerIds = mapMaybe mkPlayerId [1 .. 4]

instance FromJSON PlayerId where
    parseJSON = withScientific "PlayerId" $ \value ->
        case mkPlayerId (round value) of
            Just playerId | fromIntegral (unPlayerId playerId) == value -> pure playerId
            _ -> fail "PlayerId must be an integer from 1 to 4"

instance ToJSON PlayerId where
    toJSON = toJSON . unPlayerId

instance ToGameColor OtherColor where
    toGameColor :: OtherColor -> GameColor
    toGameColor (OtherColor c) = c

mkPlayerColor :: GameColor -> Maybe PlayerColor
mkPlayerColor c | c `elem` colorList = Just (PlayerColor c)
                | otherwise = Nothing
    where colorList = [Red, Green, Blue, Purple]

allPlayerColors :: [PlayerColor]
allPlayerColors = mapMaybe mkPlayerColor allGameColors   

mkClueColor :: GameColor -> Maybe ClueColor
mkClueColor c | c `elem` colorList = Just (ClueColor c)
              | otherwise = Nothing
    where colorList = [White, Black, Yellow, LightRed]

allClueColors :: [ClueColor]
allClueColors = mapMaybe mkClueColor allGameColors

-- | For PalmTree, Statue, and Hut, the recursive Feature bottoms out in 
-- | One of the terrain types: River, Mountain, Jungle, Beach
data Feature
    = Ocean
    | Lagoon
    | River
    | Mountain
    | Jungle
    | Beach
    | Meadow
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

allFeatures :: [Feature]
allFeatures = [Ocean, Lagoon, River, Mountain, Jungle, Beach, Meadow]

data ClueObject = FeatureClue Bool Feature | TokenClue TerrainToken | StatueClue
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data ClueCard
    = HiddenClue
    | WithinStepsOf Int ClueObject
    | NotWithinStepsOf Int ClueObject
    | IsOn ClueObject
    | IsNotOn ClueObject
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data TreasureCard
    = HiddenTreasure | Treasure Int | Curse
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data PlayerDescription = PlayerDescription
    { playerId :: PlayerId
    , playerName :: Text
    , playerAI :: Text
    , playerColor :: PlayerColor
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Score = CurrentScore Int | WinnerScore Int
    deriving (Show, Ord, Eq, Generic, FromJSON, ToJSON)

data PlayerState = PlayerState
    { player :: PlayerDescription
    , clues :: [ClueCard]
    , amulets :: Int
    , foundTreasures :: [TreasureCard]
    , availableJeepMoves :: Int
    , availableCluePlays :: Int
    , availableRemoveMarkers :: Int
    , availablePickupAmulet :: Int
    , availableClueCardExchange :: Int
    , score :: Score
    , viewingTreasures :: [TreasureCard]
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data TerrainToken 
    = PlayerJeep PlayerId 
    | ClueToken ClueColor 
    | Amulet
    | Hut
    | PalmTree
    | Statue HourHand
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data TerrainHex = TerrainHex Bool Feature [TerrainToken]
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

type ClueBoard = [(PlayerId, ClueCard)]

type HexBoard = CubeCoordinateTokens Int TerrainHex

type Deck a = ([a], [a])

type TreasureBoard = (ClueColor, ClueBoard)

-- | State for a Cursed Treasure game.
data GameState = GameState
    { players :: [PlayerState]
    , turn :: Int
    , playerTurn :: PlayerId -- playerTurn is the global player turn
    , activePlayer :: PlayerId -- activePlayer is the player that must make a choice
    , clueDeck :: Deck ClueCard
    , treasureDeck :: Deck TreasureCard
    , terrainBoard :: HexBoard
    , treasureBoards :: [TreasureBoard]
    , raisingTreasure :: Maybe RaisingTreasureState
    , latestMessage :: Text
    , gameOver :: Bool
    , seed :: (Int, Int)
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

class ToGameState a where
  toGameState :: a -> GameState

instance ToGameState GameState where
    toGameState :: GameState -> GameState
    toGameState = id

newtype CensoredGameState = CensoredGameState GameState
    deriving stock (Show, Eq, Generic)
    deriving newtype (ToJSON)

mkCensoredGameState :: GameState -> PlayerId -> CensoredGameState
mkCensoredGameState g viewerId =
    CensoredGameState $
        g
            { players = map (censorPlayer viewerId) g.players
            , clueDeck = censorClueDeck g.clueDeck
            , treasureDeck = censorTreasureDeck g.treasureDeck
            , raisingTreasure = censorRaisingTreasure <$> g.raisingTreasure
            , seed = (0, 0)
            }
  where
    censorPlayer viewId ps
        | ps.player.playerId == viewId = ps
        | otherwise =
            ps
                { clues = map (const HiddenClue) ps.clues
                , viewingTreasures = map (const HiddenTreasure) ps.viewingTreasures
                }

    censorClueDeck (drawPile, discardPile) = (map (const HiddenClue) drawPile, discardPile)

    censorTreasureDeck (drawPile, discardPile) =
        (map (const HiddenTreasure) drawPile, discardPile)

censorRaisingTreasure :: RaisingTreasureState -> RaisingTreasureState
censorRaisingTreasure rt = rt
    { rtTreasureChest = first (showDeckOpt (null rt.rtViewing)) rt.rtTreasureChest }
    where   showDeckOpt True (t:rest) = t : map (const HiddenTreasure) rest
            showDeckOpt _ d = map (const HiddenTreasure) d

instance ToGameState CensoredGameState where
    toGameState :: CensoredGameState -> GameState
    toGameState (CensoredGameState g) = g

data RaisingTreasureState = RaisingTreasureState
    { rtTreasureChest :: Deck TreasureCard
    , rtOrder :: [PlayerId]
    , rtPlayerIndex :: Int
    , rtViewing :: [PlayerId]
    } 
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Player Move for Cursed Treasure game.
data PlayerMove = PlayerMoveError Text
    | PassTurn
    | PlayClue ClueColor ClueCard
    | MoveJeep Int Int
    | ExchangeClueCards
    | PickupAmulet
    | UseAmuletIncrMove
    | UseAmuletPlayClue ClueColor ClueCard 
    | UseAmuletExchangeCards
    | UseAmuletRemoveSiteMarker ClueColor Int Int
    | RaiseTreasure ClueColor
    | RaisingTreasurePass
    | RaisingTreasureTake
    | RaisingTreasureWardCurse
    | RaisingTreasureAcceptCurse
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data GameMode = GameModeNominal
    | GameModeError Text
    | GameModeMustMoveJeep
    | GameModeRaisingTreasureView RaisingTreasureState
    | GameModeRaisingTreasureChoice RaisingTreasureState
    deriving (Show, Eq, Generic, FromJSON, ToJSON)
