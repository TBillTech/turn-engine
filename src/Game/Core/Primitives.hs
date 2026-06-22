{-# LANGUAGE DerivingStrategies #-}
module Game.Core.Primitives
    ( PlayerDescription (..)
    , PlayerId
    , mkPlayerId
    , unPlayerId
    , allPlayerIds
    , GameColor (..)
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
    , cubeCoordinateDistFloor
    , toXYScaledOrientation
    , radiusOneCubeCoordinates
    , radiusTwoCubeCoordinates
    , SeedStream (..)
    , mkSeedStream
    , seedStreamStdGen
    , nextSeedStream
    , Context (..)
    , Hoist
    , hoist
    , PropertySet (..)
    , PropertySetHandle
    , VoxelPropertyValue (..)
    , propertySetUnion
    , propertySetDifference
    , propertySetInsert
    , propertySetDelete
    , propertySetLookup
    , propertyTryLookup
    , Voxel
    , PropertySets (..)
    , PropertySetHashmap (..)
    , PropertyGroups (..)
    , VoxelSpecifier (..)
    , VoxelSelection (..)
    , ToolApplication (..)
    )
where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=), object, withObject, withScientific)
import qualified Data.Map.Strict as Map
import System.Random (StdGen, mkStdGen)

data PlayerDescription = PlayerDescription
    { playerRuleset :: Text
    , playerId :: PlayerId
    , playerName :: Text
    , playerAI :: Text
    , playerColor :: GameColor
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype PlayerId = PlayerId Int
    deriving stock (Show, Eq, Ord, Generic)

mkPlayerId :: Int -> Maybe PlayerId
mkPlayerId playerId
    | playerId >= 1 = Just (PlayerId playerId)
    | otherwise = Nothing

unPlayerId :: PlayerId -> Int
unPlayerId (PlayerId playerId) = playerId

allPlayerIds :: [PlayerId]
allPlayerIds = mapMaybe mkPlayerId [1 ..]

instance FromJSON PlayerId where
    parseJSON = withScientific "PlayerId" $ \value ->
        case mkPlayerId (round value) of
            Just playerId | fromIntegral (unPlayerId playerId) == value -> pure playerId
            _ -> fail "PlayerId must be a positive integer"

instance ToJSON PlayerId where
    toJSON = toJSON . unPlayerId

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

cubeCoordinateDistFloor :: Integral c => CubeCoordinate c -> CubeCoordinate c -> c
cubeCoordinateDistFloor coordA coordB = floor (cubeCoordinateDistance coordA coordB)

isUnitCubeDist :: Real c => CubeCoordinate c -> CubeCoordinate c -> Bool
isUnitCubeDist (CubeCoordinate (q_a, r_a)) (CubeCoordinate (q_b, r_b)) =
    dq*dq + dq*dr + dr*dr == 1
    where   dq = q_a - q_b
            dr = r_a - r_b

-- | Serializable RNG state represented as (base seed, stream index).
data SeedStream = SeedStream
    { seedBase :: Int
    , seedIndex :: Int
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON SeedStream where
    toJSON (SeedStream base index) = toJSON (base, index)

instance FromJSON SeedStream where
    parseJSON value = do
        (base, index) <- parseJSON value
        pure (SeedStream base index)

mkSeedStream :: Int -> Int -> SeedStream
mkSeedStream = SeedStream

nextSeedStream :: SeedStream -> SeedStream
nextSeedStream (SeedStream base index) = SeedStream base (index + 1)

-- | Derives a deterministic generator in O(1) from the serialized RNG stream state.
seedStreamStdGen :: SeedStream -> StdGen
seedStreamStdGen (SeedStream base index) = mkStdGen mixed
    where
        mixed = fromInteger ((a * 6364136223846793005 + b * 1442695040888963407 + 11400714819323198485) `mod` 2147483647)
        a = toInteger base
        b = toInteger index

data Context stateType = Context
    { previousCommittedState :: Maybe stateType
    , currentState :: stateType
    , planningState :: Maybe stateType
    }

instance Functor Context where
    fmap :: (a -> b) -> Context a -> Context b
    fmap f ctx = Context
        { previousCommittedState = f <$> ctx.previousCommittedState
        , currentState = f ctx.currentState
        , planningState = f <$> ctx.planningState
        }

class Hoist e1 e2 where
    hoist :: e1 -> e2

instance Hoist e1 e2 => Hoist [e1] [e2] where
    hoist = map hoist

instance Hoist e1 e2 => Hoist (Either e1 a) (Either e2 a) where
    hoist = first hoist

-- **** Plain Voxel definition ****
-- We take it that a property name is a Text, and we define the property value as a 
-- limited set of options:
data VoxelPropertyValue = NullProperty
    | TrueProperty
    | IntProperty Int
    | DoubleProperty Double
    | TextProperty Text
    | ListProperty [VoxelPropertyValue]
    | KeyValueProperty PropertySet
    deriving (Show, Eq, Generic)

instance Ord VoxelPropertyValue where
    NullProperty `compare` NullProperty = False `compare` False
    NullProperty `compare` _ = False `compare` True
    _ `compare` NullProperty = True `compare` False
    TrueProperty `compare` TrueProperty = True `compare` True
    _ `compare` TrueProperty = False `compare` True
    TrueProperty `compare` _ = True `compare` False
    (IntProperty a) `compare` (IntProperty b) = a `compare` b
    (DoubleProperty a) `compare` (IntProperty b) = a `compare` fromIntegral b
    (IntProperty a) `compare` (DoubleProperty b) = fromIntegral a `compare` b
    (IntProperty _) `compare` _ = False `compare` True
    (DoubleProperty _) `compare` _ = False `compare` True
    _ `compare` (IntProperty _) = True `compare` False
    _ `compare` (DoubleProperty _) = True `compare` False
    (TextProperty a) `compare` (TextProperty b) = a `compare` b
    (TextProperty _) `compare` _ = False `compare` True
    _ `compare` (TextProperty _) = True `compare` False
    (ListProperty a) `compare` (ListProperty b) = a `compare` b
    (ListProperty _) `compare` _ = False `compare` True
    _ `compare` (ListProperty _) = True `compare` False
    (KeyValueProperty a) `compare` (KeyValueProperty b) = a `compare` b

-- Then the key-value labeled set of properties is nothing but a Text to VoxelPropertyValue map:
newtype PropertySet = PropertySet (Map Text VoxelPropertyValue)
    deriving (Ord, Show, Eq, Generic)

propertySetUnion :: PropertySet -> PropertySet -> PropertySet
propertySetUnion (PropertySet left) (PropertySet right) = PropertySet (left <> right)

propertySetInsert :: Text -> VoxelPropertyValue -> PropertySet -> PropertySet
propertySetInsert key value (PropertySet properties) = PropertySet (Map.insert key value properties)

propertySetDifference :: PropertySet -> PropertySet -> PropertySet
propertySetDifference (PropertySet left) (PropertySet right) = PropertySet (Map.difference left right)

propertySetDelete :: Text -> PropertySet -> PropertySet
propertySetDelete key (PropertySet properties) = PropertySet (Map.delete key properties)

propertySetLookup :: Text -> PropertySet -> VoxelPropertyValue
propertySetLookup key (PropertySet properties) = fromMaybe NullProperty $ Map.lookup key properties

propertyTryLookup :: Text -> Maybe PropertySet -> VoxelPropertyValue
propertyTryLookup key (Just (PropertySet properties)) = fromMaybe NullProperty $ Map.lookup key properties
propertyTryLookup _ Nothing = NullProperty

-- In Haskell, data structures are shared, so there will be no problem
-- keeping the PropertySet directly in the Voxel even multiplied by millions of references, 
-- When creating the view to be serialized, to resolve dereferencing, we will
-- remap the layers to contain handles which can be used to look up a PropertySet.
-- This does imply we should try to assign the same PropertySet to voxels over and over, not
-- rebuild PropertySets over and over, when possible. To accellerate searching for logically
-- identical Voxels, producing the view will require the PropertySetHashmap.

-- Notionally, a HexVoxel is defined like the following data structure:
-- data HexVoxel = HexVoxel {
--     q :: Int, -- Cube Coordinate q
--     r :: Int, -- Cube Coordinate r
--     b :: Int, -- The Bottom altitude of the hex in z
--     t :: Int, -- The Top altitude of the hex in z
--     enabled :: Bool, -- Whether the HexVoxel is enabled for the current Tool
--     -- And then each voxel has to contain an arbitrary set of additional properties
--     properties :: PropertySet
-- }
-- HOWEVER, "rendering" a view is obviously going to be walking the indexes, or presenting the voxels
-- sparsely like in the SparseHexVoxels defined below, so we don't need to track q and r within the
-- HexVoxel itself.  Moreover, the bottom altitude and the top altitude are conditionally needed for 
-- hexes NOT simply on the 0 z plane. Moreover, the HexVoxel being _enabled_ is ALSO a property that 
-- properly only belongs to some hex voxels, while most others can be disabled by default. Since enabled 
-- with value (TrueProperty) can be handled as a mutation of the PropertySet, it turns
-- out nothing remains in the HexVoxel data structure _except_ the properties:
-- type HexVoxel = PropertySet
-- And since there is nothing at this point distinguishing it from an arbitrary geometry Voxel:
type Voxel = PropertySet

-- **** VIEW Voxel Representation ****
-- We envision for this architecture that the number of HexVoxel objects could be grids of 
-- perhaps 3600 by 3600. Thus the HexVoxel _view_ should be lightweight versus properties, and we
-- should be able to compress it using property handles. So we define a property set handle
-- which is basically and notionally close to an index into an array of property sets:
type PropertySetHandle = Int

-- Now each Voxel in the _view_ (which gets serialized) should not literally contain a PropertySet 
-- because many terrain voxels or voxels part of a larger structure will have identical property sets. 
-- So we need another map to intern the property sets using the PropertySetHandle when constructing the view:
newtype PropertySets = PropertySets (Map PropertySetHandle PropertySet)
    deriving (Show, Eq, Generic)

-- Morover, when in order to effciently compare a Voxel with others and to detect identical voxels,
-- we will need to map a property set hash back to candidate equal PropertySetHandles:
type PropertySetHash = Int
newtype PropertySetHashmap = PropertySetHashedHandles (Map PropertySetHash [PropertySetHandle])
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)

-- **** PropertySet Construction ****
-- Now to build up useful PropertySets, we will wish to compose named property groups.
-- So for the convenience of PropertySet constructions we define:
type PropertyGroupName = Text
newtype PropertyGroups = PropertyGroups (Map PropertyGroupName PropertySet)
    deriving stock (Show, Eq, Generic)

-- For the user to actually apply a tool though, we need to define a selection of Voxels. There are 
-- arguments to be made for both sparse selections AND block selections, so support both. Note that
-- the (Int, Int) payload is intended to carry (top, bottom) range for verticality.
data VoxelSpecifier = NoVoxelSpecifier 
    | VerticalVoxelSpecifier Int Int 
    | PropertyVoxelSpecifier Text
    | IndexVoxelSpecifier Int
    | ANDVoxelSpecifier VoxelSpecifier VoxelSpecifier
    deriving (Show, Eq, Generic)
type VoxelLayerIndex = Int
type FirstRowIndex = Int
type FirstColumnIndex = Int
data VoxelSelection = SparseSelection VoxelLayerIndex (Map (CubeCoordinate Int) VoxelSpecifier)
    | DenseSelection VoxelLayerIndex FirstRowIndex [(FirstColumnIndex, [VoxelSpecifier])]
    deriving (Show, Eq, Generic)

-- To fully apply a tool then is captured in a ToolApplication:
data ToolApplication = ToolApplication {
    toolLayer :: Int,
    appliedTool :: (CubeCoordinate Int, VoxelSpecifier),
    appliedSelection :: VoxelSelection
} deriving (Show, Eq, Generic)

