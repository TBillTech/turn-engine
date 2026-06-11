{-|
Module: Game.VoxelVerse.Layout
Description: Human-editable hex grid layout literals with odd-r/even-r offset support.

Parses a list of row strings (whitespace-separated tokens) into axial cube
coordinates suitable for use with 'CubeCoordinateTokens'.  Each token
represents a named hex descriptor looked up in a 'LayoutDictionary'.

Coordinate convention
---------------------
Hexes are pointy-top.  Row offset selects whether odd-numbered rows (0-based)
or even-numbered rows are shifted right (+col direction) in the authored text.
Leading whitespace in a row string is ignored for tokenisation; the visual
indent is purely cosmetic and does NOT affect which column a token occupies.
The offset semantics come entirely from the 'RowOffset' field of 'LayoutSpec'.

Odd-r conversion (default)
---------------------------
@
  r even:  ... col-1  col  col+1 ...
  r odd:      ... col-1  col  col+1 ...   (shifted right by half a cell)
@

Axial formula:
@
  q = (col - anchorCol) - (r \`div\` 2)
  r = row  - anchorRow
@

  q = (col - anchorCol) - (row `div` 2)

Usage example
-------------
> import Game.VoxelVerse.Layout
> import qualified Data.Map.Strict as Map
>
> data Terrain = Meadow | Jungle | Beach deriving Show
>
> islandLayout :: [Text]
> islandLayout =
>     [ "mdo   jng   mdo"
>     , "    bch   mdo"     -- leading indent ignored; row offset from rowOffset
>     , "mdo   bch   jng"
>     ]
>
> islandDict :: LayoutDictionary Terrain
> islandDict = Map.fromList [("mdo", Meadow), ("jng", Jungle), ("bch", Beach)]
>
> result :: Either [LayoutError] [(CubeCoordinate Int, Terrain)]
> result = hexLayoutToCoordinates defaultLayoutSpec islandDict islandLayout
-}
module Game.VoxelVerse.Layout
    ( -- * Specification
      RowOffset (..)
    , LayoutSpec (..)
    , defaultLayoutSpec
      -- * Dictionary
    , LayoutDictionary
    , makeDictionary
      -- * Errors
    , LayoutError (..)
      -- * Parsed cells
    , ParsedHexCell (..)
      -- * Core operations
    , parseHexLayout
    , concatHexRows
    , mapHexLayout
      -- * Bridge helpers
    , toCubeCoordinates
    , toCubeCoordinateTokens
    , hexLayoutToCoordinates
    ) where

import qualified Data.Map.Strict as Map
import Game.Core.Primitives
    ( CubeCoordinate
    , CubeCoordinateTokens (..)
    , HourHand
    , mkCubeCoordinate
    )

-- ---------------------------------------------------------------------------
-- Specification
-- ---------------------------------------------------------------------------

-- | Which rows are shifted right (in the +column direction) in the
-- pointy-top hex layout.
data RowOffset
    = OddR   -- ^ Odd-numbered rows (1, 3, 5, …) are shifted right.
    | EvenR  -- ^ Even-numbered rows (0, 2, 4, …) are shifted right.
    deriving (Show, Eq)

-- | Full specification for interpreting a hex layout literal.
data LayoutSpec = LayoutSpec
    { rowOffset   :: RowOffset
      -- ^ Row-shift convention.
    , emptyToken  :: Maybe Text
      -- ^ A token that represents an absent cell (skipped; not added to output).
    , strictWidth :: Maybe Int
      -- ^ When @Just n@, every row must tokenise to exactly @n@ cells;
      --   any deviation produces a 'RowLengthMismatch' errorLists
    , anchorCol   :: Int
      -- ^ Column index (0-based) that maps to @q = 0@.  Default @0@.
    , anchorRow   :: Int
      -- ^ Row index (0-based) that maps to @r = 0@.  Default @0@.
    }
    deriving (Show, Eq)

-- | Sensible defaults: 'OddR', no empty token, no width constraint,
--   origin at the top-left cell.
defaultLayoutSpec :: LayoutSpec
defaultLayoutSpec = LayoutSpec
    { rowOffset   = OddR
    , emptyToken  = Nothing
    , strictWidth = Nothing
    , anchorCol   = 0
    , anchorRow   = 0
    }

-- ---------------------------------------------------------------------------
-- Dictionary
-- ---------------------------------------------------------------------------

-- | A map from authored token strings to descriptor values.
type LayoutDictionary descriptor = Map Text descriptor

-- | Build a 'LayoutDictionary' from a list of @(token, descriptor)@ pairs.
--   Returns 'Left' with a 'DuplicateDictionaryKey' errorLists
--   for every key that
--   appears more than once; returns 'Right' when all keys are unique.
makeDictionary
    :: [(Text, descriptor)]
    -> Either [LayoutError] (LayoutDictionary descriptor)
makeDictionary pairs =
    let countMap = Map.fromListWith (+) [(k, 1 :: Int) | (k, _) <- pairs]
        dups     = Map.keys $ Map.filter (> 1) countMap
    in  if null dups
            then Right (Map.fromList pairs)
            else Left  (map DuplicateDictionaryKey dups)

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Structured errors produced by parsing and mapping operations.
data LayoutError
    = RowLengthMismatch
        { leRow      :: Int   -- ^ 0-based row index.
        , leExpected :: Int   -- ^ Expected cell count (from 'strictWidth').
        , leActual   :: Int   -- ^ Actual cell count after tokenisation.
        }
    | UnknownToken
        { leRow   :: Int    -- ^ 0-based row index.
        , leCol   :: Int    -- ^ 0-based column index within the row.
        , leToken :: Text   -- ^ The unrecognised token string.
        }
    | DuplicateDictionaryKey
        { leToken :: Text   -- ^ Duplicate key.
        }
    | EmptyLayout
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Parsed cells
-- ---------------------------------------------------------------------------

-- | A single hex cell as authored, carrying both the offset grid position
--   (@cellRow@, @cellCol@) and the derived axial coordinates (@cellQ@, @cellR@).
data ParsedHexCell = ParsedHexCell
    { cellRow   :: Int    -- ^ 0-based row index in the authored grid.
    , cellCol   :: Int    -- ^ 0-based column index within the row.
    , cellQ     :: Int    -- ^ Axial q coordinate.
    , cellR     :: Int    -- ^ Axial r coordinate.
    , cellToken :: Text   -- ^ The authored token string.
    }
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Core operations
-- ---------------------------------------------------------------------------

-- | Parse a list of row strings into 'ParsedHexCell' values.
--
--   Each row is tokenised by splitting on whitespace; leading, trailing, and
--   internal whitespace is discarded so visual indentation does not affect
--   column numbering.  Cells whose token equals 'emptyToken' are silently
--   dropped.
--
--   Returns @'Left' errors@ (accumulated) if 'strictWidth' is set and any row
--   violates it; returns @'Right' cells@ otherwise.
parseHexLayout :: LayoutSpec -> [Text] -> Either [[LayoutError]] [[ParsedHexCell]]
parseHexLayout _    []   = Left [[EmptyLayout]]
parseHexLayout spec rows =
    let indexed  = zip [0..] (map words rows)
        errors   = map (widthErrors spec) indexed
        cells    = map (rowToCells  spec) indexed
    in  if not (all null errors) then Left errors else Right cells

concatHexRows :: Either [[LayoutError]] [[ParsedHexCell]] -> Either [LayoutError] [ParsedHexCell]
concatHexRows = bimap concat concat

widthErrors :: LayoutSpec -> (Int, [Text]) -> [LayoutError]
widthErrors LayoutSpec{ strictWidth = Just w } (rowIdx, toks)
    | length toks /= w = [RowLengthMismatch rowIdx w (length toks)]
widthErrors _ _ = []

rowToCells :: LayoutSpec -> (Int, [Text]) -> [ParsedHexCell]
rowToCells spec (rowIdx, toks) =
    [ buildCell spec rowIdx colIdx tok
    | (colIdx, tok) <- zip [0..] toks
    , Just tok /= spec.emptyToken
    ]

buildCell :: LayoutSpec -> Int -> Int -> Text -> ParsedHexCell
buildCell spec rowIdx colIdx tok =
    let r = rowIdx - spec.anchorRow
        c = colIdx - spec.anchorCol
        q = case spec.rowOffset of
            OddR  -> c - rowIdx `div` 2
            EvenR -> c - (rowIdx + 1) `div` 2
    in  ParsedHexCell
        { cellRow   = rowIdx
        , cellCol   = colIdx
        , cellQ     = q
        , cellR     = r
        , cellToken = tok
        }

-- A CellRunSearch is a list of Text to match in the rows of the ParsedHexCells. 
type CellRunSearch a = ([Text], a)
-- When a CellRunSearch is found, we construct a CellRunMatch to track the matched cells plus payload.
type CellRunMatch a = ([ParsedHexCell], a)

data CellSearchError
    = OverlappingCellSearch
        { leRow :: Int      -- ^ 0-based row index of first collision cell
        , leCol :: Int            -- ^ 0-based column index of first collision cell
        }
    | SearchCountWrongError
        { cpExpected :: Int -- ^ count of expected Cell Search matches
        , cpActual   :: Int -- ^ actual count of Cell Search matches
        }
    deriving (Show, Eq)

-- | Find and track CellRunSearch on a row of ParsedHexCells
--
--   Searches each row for a sequence of ParsedHexCells that matches the list of the fst of the
--   CellRunSearch tuples. If the row matches more than one time, and if any two matches overlap,
--   creates a CellSearchError.  Also creates a CellSearchError if the wrong number of matches is found.
findRowCellMatches :: (Maybe Int, CellRunSearch a) -> (Int, [ParsedHexCell])
    -> Either [CellSearchError] [(Int, CellRunMatch a)]
findRowCellMatches (mCount, search) (rowId, cellRow)
    | null errors = Right $ map (second (,payload)) matches
    | otherwise = Left errors
    where   toFind = fst search
            payload = snd search
            fLength = length toFind
            possibleLocs = [0..(length cellRow - fLength)]
            candidates = map toCandidate possibleLocs
            toCandidate i = (i, take fLength $ drop i cellRow)
            matches = filter (matchTokens toFind . snd) candidates
            matchTokens f b = f == map (.cellToken) b
            overlaps = filter ((fLength >= ) . uncurry (-) . bimap fst fst) $ zip (drop 1 matches) matches
            overlapErrors = map ((\o -> OverlappingCellSearch { leRow = rowId, leCol = o}) . fst . fst) overlaps
            countError Nothing _ = []
            countError (Just c) mC | c == mC = []
                                   | otherwise = [SearchCountWrongError {cpExpected = c, cpActual = mC}]
            errors = countError mCount (length matches) ++ overlapErrors

findRowMixedCellMatches :: [(Maybe Int, CellRunSearch a)] -> (Int, [ParsedHexCell])
    -> Either [CellSearchError] [(Int, CellRunMatch a)]
findRowMixedCellMatches countSearches (rowId, cellRow)
        | null errors = Right allMatches
        | otherwise = Left errors
    where   eMixedCellMatches = map (`findRowCellMatches` (rowId, cellRow)) countSearches
            (errorLists, rowMatches) = partitionEithers eMixedCellMatches
            allMatches = sortWith fst $ concat rowMatches
            blockTuples = map (second (length . fst)) allMatches
            overlaps collisions [] = collisions
            overlaps collisions ((b,e):rest) = overlaps (curCollisions (b+e) rest ++ collisions) rest
            curCollisions _ [] = []
            curCollisions e (block:rest) | fst block <= e = block:curCollisions e rest
                                         | otherwise = []
            overlapErrors = map ((\o -> OverlappingCellSearch { leRow = rowId, leCol = o}) . fst) 
                $ overlaps [] blockTuples
            errors = concat errorLists ++ overlapErrors

findCellMatches :: Maybe Int -> CellRunSearch a -> [[ParsedHexCell]]
    -> Either [CellSearchError] [CellRunMatch a]
findCellMatches mCount search cellRows
        | null errors = Right $ map snd allMatches
        | otherwise = Left errors
    where   eRowMatches = zipWith (curry (findRowCellMatches (Nothing, search))) [0..1] cellRows
            (errorLists, rowMatches) = partitionEithers eRowMatches
            allMatches = concat rowMatches
            countError Nothing _ = []
            countError (Just c) mC | c == mC = []
                                   | otherwise = [SearchCountWrongError {cpExpected = c, cpActual = mC}]
            errors = countError mCount (length allMatches) ++ concat errorLists

findAllCellMatches :: [CellRunSearch a] -> [[ParsedHexCell]]
    -> Either [CellSearchError] [CellRunMatch a]
findAllCellMatches searches cellRows
        | null errors = Right $ map snd allMatches
        | otherwise = Left errors
    where   eRowMatches = zipWith (curry (findRowMixedCellMatches $ map (Nothing, ) searches)) [0..1] cellRows
            (errorLists, rowMatches) = partitionEithers eRowMatches
            allMatches = concat rowMatches
            errors = concat errorLists


-- | Map parsed hex cells to @(cell, descriptor)@ pairs using a dictionary.
--
--   Accumulates 'UnknownToken' errors for every cell whose token is absent
--   from the dictionary, then returns either all errors or all pairs.
mapHexLayout
    :: LayoutDictionary d
    -> [ParsedHexCell]
    -> Either [LayoutError] [(ParsedHexCell, d)]
mapHexLayout dict cells =
    let (errors, pairs) = partitionEithers (map (lookupCell dict) cells)
    in  if null errors then Right pairs else Left errors

lookupCell
    :: LayoutDictionary d
    -> ParsedHexCell
    -> Either LayoutError (ParsedHexCell, d)
lookupCell dict cell =
    case Map.lookup cell.cellToken dict of
        Just d  -> Right (cell, d)
        Nothing -> Left UnknownToken
            { leRow   = cell.cellRow
            , leCol   = cell.cellCol
            , leToken = cell.cellToken
            }

-- ---------------------------------------------------------------------------
-- Bridge helpers
-- ---------------------------------------------------------------------------

-- | Convert @(ParsedHexCell, descriptor)@ pairs to @(CubeCoordinate Int, descriptor)@ pairs.
toCubeCoordinates :: [(ParsedHexCell, d)] -> [(CubeCoordinate Int, d)]
toCubeCoordinates = map (\(cell, d) -> (mkCubeCoordinate cell.cellQ cell.cellR, d))

-- | Wrap coordinate pairs in a 'CubeCoordinateTokens' with a caller-supplied orientation.
--   When duplicate axial coordinates arise (same q,r from different cells),
--   the last occurrence in the list wins, matching 'Map.fromList' semantics.
toCubeCoordinateTokens
    :: HourHand
    -> [(ParsedHexCell, d)]
    -> CubeCoordinateTokens Int d
toCubeCoordinateTokens orientation pairs =
    CubeCoordinateTokens orientation (Map.fromList (toCubeCoordinates pairs))

-- | Convenience combinator: parse rows, map tokens to descriptors, and
--   return axial coordinate pairs.  Equivalent to:
--
-- > parseHexLayout spec rows >>= mapHexLayout dict >>= pure . toCubeCoordinates
hexLayoutToCoordinates
    :: LayoutSpec
    -> LayoutDictionary d
    -> [Text]
    -> Either [LayoutError] [(CubeCoordinate Int, d)]
hexLayoutToCoordinates spec dict rows = do
    cells <- concatHexRows $ parseHexLayout spec rows
    pairs <- mapHexLayout dict cells
    pure (toCubeCoordinates pairs)

