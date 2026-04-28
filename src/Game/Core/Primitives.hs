{-# LANGUAGE DerivingStrategies #-}
module Game.Core.Primitives
    ( GameColor (..)
    , ToGameColor (..)
    , allGameColors
    , HourHand
    , mkHourHand
    , ToHourHand (..)
    , FromHourHand (..)
    , CubeCoordinate
    , mkCubeCoordinate
    , toPair
    , CubeCoordinateTokens (..)
    , adjacentCubeCoordinates
    , isUnitCubeDist
    , cubeCoordinateDistance
    , toXYScaledOrientation
    , radiusOneCubeCoordinates
    , radiusTwoCubeCoordinates
    )
where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=), object, withObject)
import qualified Data.Map.Strict as Map

data GameColor = White
    | LightRed | Red | DarkRed
    | LightGreen | Green | DarkGreen
    | LightBlue | Blue | DarkBlue
    | LightMagenta | Magenta | DarkMagenta
    | LightCyan | Cyan | DarkCyan
    | LightYellow | Yellow | DarkYellow
    | LightOrange | Orange | DarkOrange -- Halfway between Red and Yellow
    | LightPurple | Purple | DarkPurple -- Halfway between Blue and Magenta
    | Gray | Black
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, Enum, Bounded)

class ToGameColor a where
  toGameColor :: a -> GameColor

instance ToGameColor GameColor where
    toGameColor :: GameColor -> GameColor
    toGameColor = id

allGameColors :: [GameColor]
allGameColors = universe

data TokenSpaceIndex = TokenSpaceTextIndex Text
    | TokenSpaceIntIndex Int
    | TokenSpace2DIndex Int Int
--    | CompositeTokenSpaceIndex TokenSpaceIndex TokenSpaceIndex
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | HouryHand is an angle measured in units of pi/6, or 30 degrees, or clock hour hand
newtype HourHand = HourHand Int
    deriving stock (Show, Eq, Ord, Generic)

mkHourHand :: Int -> Maybe HourHand
mkHourHand h
    | h >= 0 && h <= 11 = Just (HourHand h)
    | otherwise = Nothing

instance ToJSON HourHand where
    toJSON hh = toJSON (fromHourHand hh :: Int)

instance FromJSON HourHand where
    parseJSON value = do
        h <- parseJSON value
        maybe (fail "HourHand must be an integer between 0 and 11") pure (mkHourHand h)

class ToHourHand a where
    toHourHand :: a -> HourHand

class FromHourHand a where
    fromHourHand :: HourHand -> a

instance ToHourHand HourHand where
    toHourHand :: HourHand -> HourHand
    toHourHand = id

instance ToHourHand Int where
    toHourHand :: Int -> HourHand
    toHourHand h = HourHand $ h `mod` 12

instance FromHourHand Int where
    fromHourHand :: HourHand -> Int
    fromHourHand (HourHand h) = h

newtype ArrayOfTokens t = ArrayOfTokens (Map.Map Int t)
    deriving stock (Show, Eq, Generic)

instance (ToJSON t) => ToJSON (ArrayOfTokens t) where
    toJSON (ArrayOfTokens tokens) = toJSON (Map.toList tokens)

instance (FromJSON t) => FromJSON (ArrayOfTokens t) where
    parseJSON value = ArrayOfTokens . Map.fromList <$> parseJSON value

newtype RectilinearTokens t = RectilinearTokens (Map.Map (Int, Int) t)
    deriving stock (Show, Eq, Generic)

instance (ToJSON t) => ToJSON (RectilinearTokens t) where
    toJSON (RectilinearTokens tokens) = toJSON (Map.toList tokens)

instance (FromJSON t) => FromJSON (RectilinearTokens t) where
    parseJSON value = RectilinearTokens . Map.fromList <$> parseJSON value

-- | Cube-coordinate storage for a hex cell.
-- |
-- | The stored payload keeps the canonical axial pair @(q, r)@ and derives the
-- | third cube coordinate as @s = -q-r@ whenever needed.
newtype CubeCoordinate c = CubeCoordinate (c, c)
    deriving stock (Show, Eq, Ord, Generic)

mkCubeCoordinate :: Num c => c -> c -> CubeCoordinate c
mkCubeCoordinate q r = CubeCoordinate (q, r)

toPair :: Num c => CubeCoordinate c -> (c, c)
toPair (CubeCoordinate p) = p

instance ToJSON c => ToJSON (CubeCoordinate c) where
    toJSON (CubeCoordinate (q, r)) = toJSON (q, r)

instance (FromJSON c, Num c) => FromJSON (CubeCoordinate c) where
    parseJSON value = do
        (q, r) <- parseJSON value
        pure (mkCubeCoordinate q r)

data CubeCoordinateTokens c t = CubeCoordinateTokens HourHand (Map.Map (CubeCoordinate c) t)
    deriving stock (Show, Eq, Generic)

instance (ToJSON c, ToJSON t) => ToJSON (CubeCoordinateTokens c t) where
    toJSON (CubeCoordinateTokens orientation tokens) =
        object
            [ "orientation" .= orientation
            , "tokens" .= Map.toList tokens
            ]

instance (FromJSON c, Num c, Ord c, FromJSON t) => FromJSON (CubeCoordinateTokens c t) where
    parseJSON = withObject "CubeCoordinateTokens" $ \o -> do
        orientation <- o .: "orientation"
        tokens <- Map.fromList <$> o .: "tokens"
        pure (CubeCoordinateTokens orientation tokens)

-- | toXYScaledOrientation uses the HourHand orientation, scale, and a CubeCoordinate to 
-- | compute the center of the Hex at CubeCoordinate as a rectilinear (X, Y) coordinate 
-- | suitable, for example, placing a hex sprite on a screen.
-- | This transform assumes that the center of the hex at CubeCoordinate (0, 0) is 
-- | the same point as the rectilinear (0, 0) origin.
-- | This transform further assumes that the coordinates CubeCoordinate (n, 0) where
-- | n are integers and are the centers of hexes along a line in the direction of the HourHand 
-- | (defined just like a clock with 12 oclock up/north), such that the hex centers are exactly
-- | scale distance apart along this line, and that the pointy tops are perpendicular to this line.   
toXYScaledOrientation :: Real c => HourHand -> Float -> CubeCoordinate c -> (Double, Double)
toXYScaledOrientation (HourHand hour) scale (CubeCoordinate (q, r)) =
    (q' * qx + r' * rx, q' * qy + r' * ry)
    where
        angle = pi / 2 - fromIntegral hour * pi / 6
        scale' = realToFrac scale
        q' = realToFrac q
        r' = realToFrac r
        qx = scale' * cos angle
        qy = scale' * sin angle
        rx = scale' * cos (angle + pi / 3)
        ry = scale' * sin (angle + pi / 3)

radiusOneCubeCoordinates :: Real c => [(c, c)]
radiusOneCubeCoordinates =
    [ (1, 0)
    , (0, 1)
    , (-1, 1)
    , (-1, 0)
    , (0, -1)
    , (1, -1)
    ]

radiusTwoCubeCoordinates :: Real c => [(c, c)]
radiusTwoCubeCoordinates =
    [ (2, 0)
    , (1, 1)
    , (0, 2)
    , (-1, 2)
    , (-2, 2)
    , (-2, 1)
    , (-2, 0)
    , (-1, -1)
    , (0, -2)
    , (1, -2)
    , (2, -2)
    , (2, -1)
    ]

adjacentCubeCoordinates :: Real c => CubeCoordinate c -> [CubeCoordinate c]
adjacentCubeCoordinates (CubeCoordinate (q, r)) = uncurry mkCubeCoordinate . bimap (+q) (+r) 
    <$> radiusOneCubeCoordinates

cubeCoordinateDistance :: Real c => CubeCoordinate c -> CubeCoordinate c -> Double
cubeCoordinateDistance (CubeCoordinate (q_a, r_a)) (CubeCoordinate (q_b, r_b)) = 
    sqrt (realToFrac $ dq*dq + dq*dr + dr*dr)
    where   dq = q_a - q_b
            dr = r_a - r_b

isUnitCubeDist :: Real c => CubeCoordinate c -> CubeCoordinate c -> Bool
isUnitCubeDist (CubeCoordinate (q_a, r_a)) (CubeCoordinate (q_b, r_b)) =
    dq*dq + dq*dr + dr*dr == 1
    where   dq = q_a - q_b
            dr = r_a - r_b
