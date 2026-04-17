{-# LANGUAGE DerivingStrategies #-}
module Game.Core.Primitives where

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
allGameColors = [minBound..maxBound]

data TokenSpaceIndex = TokenSpaceTextIndex Text
    | TokenSpaceIntIndex Int
    | TokenSpace2DIndex Int Int
--    | CompositeTokenSpaceIndex TokenSpaceIndex TokenSpaceIndex
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | HouryHand is an angle measured in units of pi/6, or 30 degrees, or clock hour hand
newtype HourHand = HourHand Int
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON)

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

rawHorizontalHexOffsets :: Bool -> [((Int -> Int, Int -> Int), Int)]
rawHorizontalHexOffsets True = [(((+1), (+1)), 1)
                               ,(((+1), id), 3)
                               ,(((+1), (+(-1))), 5)
                               ,((id, (+(-1))), 7)
                               ,(((+(-1)), id), 9)
                               ,((id, (+1)), 11)]
rawHorizontalHexOffsets False = [(((+ 1), id), 1)
                                ,((id, (+1)), 3)
                                ,(((+(-1)), id), 5)
                                ,(((+(-1)), (+(-1))), 7)
                                ,(((+(-1)), id), 9)
                                ,(((+(-1)), (+1)), 11)]
applyHorizontalHexOffsets :: TokenSpaceIndex -> [(TokenSpaceIndex, HourHand)]
applyHorizontalHexOffsets (TokenSpace2DIndex i j) = map a $ rawHorizontalHexOffsets p
    where p = even j
          a ((f_i, f_j), h) = (TokenSpace2DIndex (f_i i) (f_j j), toHourHand h)
applyHorizontalHexOffsets _ = []

rawVerticalHexOffsets :: (Num c1, Num c2) => Bool -> [((c1 -> c1, c2 -> c2), Int)]
rawVerticalHexOffsets True = [((id, (+1)), 0)
                             ,(((+1), id), 2)
                             ,(((+1), (+(-1))), 4)
                             ,((id, (+(-1))), 6)
                             ,(((+(-1)), (+(-1))), 8)
                             ,(((+(-1)), id), 10)]
rawVerticalHexOffsets False = [((id, (+1)), 0)
                              ,(((+1), (+1)), 2)
                              ,(((+1), id), 4)
                              ,((id, (+(-1))), 6)
                              ,(((+(-1)), id), 8)
                              ,(((+(-1)), (+1)), 10)]
applyVerticalHexOffsets :: TokenSpaceIndex -> [(TokenSpaceIndex, HourHand)]
applyVerticalHexOffsets (TokenSpace2DIndex i j) = map a $ rawVerticalHexOffsets p
    where p = even i
          a ((f_i, f_j), h) = (TokenSpace2DIndex (f_i i) (f_j j), toHourHand h)
applyVerticalHexOffsets _ = []

adjacentIndices :: AdjacencyPolicy -> TokenSpaceIndex -> [TokenSpaceIndex]
adjacentIndices NonAdjacency _ = []
adjacentIndices _ (TokenSpaceTextIndex _) = []
adjacentIndices _ (TokenSpaceIntIndex i) = [TokenSpaceIntIndex $ i-1, TokenSpaceIntIndex $ i+1]
adjacentIndices IndexAdjacency (TokenSpace2DIndex i j)
    = map (uncurry TokenSpace2DIndex) [(i,j+1), (i+1, j), (i, j-1), (i-1, j)]
adjacentIndices HorizontalHexAdjacency ti = map fst $ applyHorizontalHexOffsets ti
adjacentIndices VerticalHexAdjacency ti = map fst $ applyVerticalHexOffsets ti

isAdjacentIndex :: AdjacencyPolicy -> TokenSpaceIndex -> TokenSpaceIndex -> Bool
isAdjacentIndex p t1 t2 = t2 `elem` adjacentIndices p t1

type QuadrupleSquares = (Int, Int)

policyTextDistance :: Text -> TokenSpaceIndex -> Maybe QuadrupleSquares
policyTextDistance _ _ = Nothing

policyIndexDistance :: TokenSpaceIndex -> TokenSpaceIndex -> Maybe QuadrupleSquares
policyIndexDistance (TokenSpaceTextIndex t) b = policyTextDistance t b
policyIndexDistance a (TokenSpaceTextIndex t) = policyTextDistance t a
policyIndexDistance (TokenSpaceIntIndex i) (TokenSpaceIntIndex j) = Just (4*(i-j)*(i-j), 0)
policyIndexDistance (TokenSpaceIntIndex i_a) (TokenSpace2DIndex i_b _)
    = Just (4*(i_a-i_b)*(i_a-i_b), 0)
policyIndexDistance (TokenSpace2DIndex _ j_a) (TokenSpaceIntIndex j_b)
    = Just (0, 4*(j_a-j_b)*(j_a-j_b))
policyIndexDistance (TokenSpace2DIndex i_a j_a) (TokenSpace2DIndex i_b j_b)
    = Just (4*(i_a-i_b)*(i_a-i_b), 4*(j_a-j_b)*(j_a-j_b))

policyHexDistance :: Bool -> TokenSpaceIndex -> TokenSpaceIndex -> Maybe QuadrupleSquares
policyHexDistance _ (TokenSpaceTextIndex t) b = policyTextDistance t b
policyHexDistance _ a (TokenSpaceTextIndex t) = policyTextDistance t a
policyHexDistance _ a@(TokenSpaceIntIndex _) b@(TokenSpaceIntIndex _)
    = policyIndexDistance a b
policyHexDistance True a@(TokenSpaceIntIndex _) b@(TokenSpace2DIndex _ _)
    = policyIndexDistance a b
policyHexDistance False a@(TokenSpaceIntIndex _) b@(TokenSpace2DIndex _ _)
    = scaleHorizontal $ policyIndexDistance a b
    where scaleHorizontal Nothing = Nothing
          scaleHorizontal (Just (isqr, jsqr)) = Just (3 * (isqr `div` 4), jsqr)
policyHexDistance False a@(TokenSpace2DIndex _ _) b@(TokenSpaceIntIndex _)
    = policyIndexDistance a b
policyHexDistance True a@(TokenSpace2DIndex _ _) b@(TokenSpaceIntIndex _)
    = scaleVertical $ policyIndexDistance a b
    where scaleVertical Nothing = Nothing
          scaleVertical (Just (isqr, jsqr)) = Just (isqr, 3 * (jsqr `div` 4))
policyHexDistance True (TokenSpace2DIndex i_a j_a) (TokenSpace2DIndex i_b j_b)
    | even j_a && even j_b = Just (4*(i_a-i_b)*(i_a-i_b), 3*(j_a-j_b)*(j_a-j_b))
    | even j_a && odd j_b = Just ((2*i_a-2*i_b-1)*(2*i_a-2*i_b-1), 3*(j_a-j_b)*(j_a-j_b))
    | odd j_a && even j_b = Just ((2*i_a-2*i_b+1)*(2*i_a-2*i_b+1), 3*(j_a-j_b)*(j_a-j_b))
    | otherwise = Just (4*(i_a-i_b)*(i_a-i_b), 3*(j_a-j_b)*(j_a-j_b))
policyHexDistance False (TokenSpace2DIndex i_a j_a) (TokenSpace2DIndex i_b j_b)
    | even i_a && even i_b = Just (3*(i_a-i_b)*(i_a-i_b), 4*(j_a-j_b)*(j_a-j_b))
    | even i_a && odd i_b = Just (3*(i_a-i_b)*(i_a-i_b), (2*j_a-2*j_b-1)*(2*j_a-2*j_b-1))
    | odd i_a && even i_b = Just (3*(i_a-i_b)*(i_a-i_b), (2*j_a-2*j_b+1)*(2*j_a-2*j_b+1))
    | otherwise = Just (3*(i_a-i_b)*(i_a-i_b), 4*(j_a-j_b)*(j_a-j_b))

policyFourSquareDistance :: AdjacencyPolicy -> TokenSpaceIndex -> TokenSpaceIndex -> Maybe QuadrupleSquares
policyFourSquareDistance NonAdjacency _ _ = Nothing
policyFourSquareDistance IndexAdjacency a b = policyIndexDistance a b
policyFourSquareDistance HorizontalHexAdjacency a b = policyHexDistance True a b
policyFourSquareDistance VerticalHexAdjacency a b = policyHexDistance False a b


-- | Defines how positions/tokens are considered adjacent.
-- | Index adjacency means along this dimension, N-1 is adjacent below to N which is adjecent below to N+1
-- | HorizontalHexAdjacency means cell (i, j) where i % 2 == 0 is adjacent 
-- | to cells (i+1, j+1), (i+1, j), (i+1, j-1), (i, j-1), (i-1, j), (i, j+1)
-- | i % 2 == 1 is adjacent to cells
-- | (i+1, j), (i, j+1), (i-1, j), (i-1, j-1), (i-1, j), (i-1, j+1)
-- | VerticalHexAdjacency means the same if you swap i and j
-- | (Not implemented) CompositeAdjacency means the first kind of adjacency indexes into the second kind
data AdjacencyPolicy
    = NonAdjacency
    | IndexAdjacency
    | HorizontalHexAdjacency
    | VerticalHexAdjacency
--    | CompositeAdjacency AdjacencyPolicy AdjacencyPolicy
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | TokenSpace is annotated by an adjacency policy and contains tokens keyed by 'k'.
data TokenSpace t = TokenSpace
    { adjacency :: AdjacencyPolicy
    , tokens :: Map.Map TokenSpaceIndex t
    }
    deriving (Show, Eq, Generic)

instance (ToJSON t) => ToJSON (TokenSpace t) where
    toJSON ts =
        object
            [ "adjacency" .= ts.adjacency
            , "tokens" .= Map.toList ts.tokens
            ]

instance (FromJSON t) => FromJSON (TokenSpace t) where
    parseJSON = withObject "TokenSpace" $ \o -> do
        adjacency <- o .: "adjacency"
        tokens <- Map.fromList <$> o .: "tokens"
        pure TokenSpace {adjacency, tokens}
