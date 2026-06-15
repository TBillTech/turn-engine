module Game.VoxelVerse.LayoutSpec where

import Test.Hspec
import Game.VoxelVerse.Layout
import Game.Core.Primitives (mkCubeCoordinate)
import Game.VoxelVerse.Types (VoxelOption (..), coordinatesToVoxelBlock)
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "defaultLayoutSpec coordinate math (odd-r)" $ do

        it "maps top-left cell to axial origin" $
            cellCoords (parseCell defaultLayoutSpec 0 0 "x") `shouldBe` (0, 0)

        it "maps same row right-neighbor to (q+1, same r)" $
            cellCoords (parseCell defaultLayoutSpec 0 1 "x") `shouldBe` (1, 0)

        it "maps row 1 col 0 to (0, 1) -- odd row, no q shift" $
            -- r=1, r`div`2=0, so q = col - 0 = col
            -- r=1, r`div`2=0, so q = col - 0 = col
            -- r=1, r`div`2=0, so q = col - 0 = col
            -- r=1, r`div`2=0, so q = col - 0 = col
            -- r=1, r`div`2=0, so q = col - 0 = col
            -- r=1, r`div`2=0, so q = col - 0 = col
            -- r=1, r`div`2=0, so q = col - 0 = col
            -- r=1, r`div`2=0, so q = col - 0 = col
            -- r=1, r`div`2=0, so q = col - 0 = col
            cellCoords (parseCell defaultLayoutSpec 1 0 "x") `shouldBe` (0, 1)

        it "maps row 1 col 1 to (1, 1)" $
            cellCoords (parseCell defaultLayoutSpec 1 1 "x") `shouldBe` (1, 1)

        it "maps row 2 col 0 to (-1, 2) -- even row, q shifts left" $
            -- r=2, r`div`2=1, so q = 0 - 1 = -1
            -- r=2, r`div`2=1, so q = 0 - 1 = -1
            -- r=2, r`div`2=1, so q = 0 - 1 = -1
            -- r=2, r`div`2=1, so q = 0 - 1 = -1
            -- r=2, r`div`2=1, so q = 0 - 1 = -1
            -- r=2, r`div`2=1, so q = 0 - 1 = -1
            -- r=2, r`div`2=1, so q = 0 - 1 = -1
            -- r=2, r`div`2=1, so q = 0 - 1 = -1
            -- r=2, r`div`2=1, so q = 0 - 1 = -1
            cellCoords (parseCell defaultLayoutSpec 2 0 "x") `shouldBe` (-1, 2)

        it "maps row 2 col 1 to (0, 2)" $
            cellCoords (parseCell defaultLayoutSpec 2 1 "x") `shouldBe` (0, 2)

        it "maps row 3 col 0 to (-1, 3) -- odd row, same q shift as row 2" $
            -- r=3, r`div`2=1, so q = 0 - 1 = -1
            -- r=3, r`div`2=1, so q = 0 - 1 = -1
            -- r=3, r`div`2=1, so q = 0 - 1 = -1
            -- r=3, r`div`2=1, so q = 0 - 1 = -1
            -- r=3, r`div`2=1, so q = 0 - 1 = -1
            -- r=3, r`div`2=1, so q = 0 - 1 = -1
            -- r=3, r`div`2=1, so q = 0 - 1 = -1
            -- r=3, r`div`2=1, so q = 0 - 1 = -1
            -- r=3, r`div`2=1, so q = 0 - 1 = -1
            cellCoords (parseCell defaultLayoutSpec 3 0 "x") `shouldBe` (-1, 3)

        it "maps row 4 col 0 to (-2, 4) -- even row, q shifts left again" $
            -- r=4, r`div`2=2, so q = 0 - 2 = -2
            -- r=4, r`div`2=2, so q = 0 - 2 = -2
            -- r=4, r`div`2=2, so q = 0 - 2 = -2
            -- r=4, r`div`2=2, so q = 0 - 2 = -2
            -- r=4, r`div`2=2, so q = 0 - 2 = -2
            -- r=4, r`div`2=2, so q = 0 - 2 = -2
            -- r=4, r`div`2=2, so q = 0 - 2 = -2
            -- r=4, r`div`2=2, so q = 0 - 2 = -2
            -- r=4, r`div`2=2, so q = 0 - 2 = -2
            cellCoords (parseCell defaultLayoutSpec 4 0 "x") `shouldBe` (-2, 4)

    describe "even-r coordinate math" $ do
        let evenSpec = defaultLayoutSpec { rowOffset = EvenR }

        it "maps row 0 col 0 to (0, 0)" $
            -- r=0, (r+1)`div`2=0, q=0
            -- r=0, (r+1)`div`2=0, q=0
            -- r=0, (r+1)`div`2=0, q=0
            -- r=0, (r+1)`div`2=0, q=0
            -- r=0, (r+1)`div`2=0, q=0
            -- r=0, (r+1)`div`2=0, q=0
            -- r=0, (r+1)`div`2=0, q=0
            -- r=0, (r+1)`div`2=0, q=0
            -- r=0, (r+1)`div`2=0, q=0
            cellCoords (parseCell evenSpec 0 0 "x") `shouldBe` (0, 0)

        it "maps row 1 col 0 to (-1, 1) -- odd row is NOT shifted in even-r" $
            -- r=1, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=1, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=1, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=1, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=1, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=1, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=1, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=1, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=1, (r+1)`div`2=1, q = 0 - 1 = -1
            cellCoords (parseCell evenSpec 1 0 "x") `shouldBe` (-1, 1)

        it "maps row 2 col 0 to (-1, 2) -- even row, same shift as row 1" $
            -- r=2, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=2, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=2, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=2, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=2, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=2, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=2, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=2, (r+1)`div`2=1, q = 0 - 1 = -1
            -- r=2, (r+1)`div`2=1, q = 0 - 1 = -1
            cellCoords (parseCell evenSpec 2 0 "x") `shouldBe` (-1, 2)

    describe "anchorRow translation keeps row stagger phase" $ do

        it "odd-r: anchorRow changes r only, not which authored rows share q-shift" $ do
            let anchored = defaultLayoutSpec { anchorRow = 2 }
            cellCoords (parseCell anchored 2 0 "x") `shouldBe` (-1, 0)
            cellCoords (parseCell anchored 3 0 "x") `shouldBe` (-1, 1)

        it "even-r: anchorRow changes r only, not which authored rows are shifted" $ do
            let anchoredEven = defaultLayoutSpec { rowOffset = EvenR, anchorRow = 1 }
            cellCoords (parseCell anchoredEven 1 0 "x") `shouldBe` (-1, 0)
            cellCoords (parseCell anchoredEven 2 0 "x") `shouldBe` (-1, 1)

    describe "parseHexLayout" $ do

        it "returns EmptyLayout for an empty list" $
            parseHexLayout defaultLayoutSpec [] `shouldBe` Left [EmptyLayout]

        it "parses a single row of tokens" $ do
            let result = concatHexRows $ parseHexLayout defaultLayoutSpec ["aaa bbb ccc"]
            fmap (map (\c -> (c.cellToken, c.cellQ, c.cellR))) result
                `shouldBe` Right [("aaa", 0, 0), ("bbb", 1, 0), ("ccc", 2, 0)]

        it "ignores leading and trailing whitespace in rows" $ do
            let result = parseHexLayout defaultLayoutSpec ["  aaa   bbb  "]
            fmap (length . concat) result `shouldBe` Right 2

        it "ignores extra internal whitespace between tokens" $ do
            let r1 = concatHexRows $ parseHexLayout defaultLayoutSpec ["a   b   c"]
            let r2 = concatHexRows $ parseHexLayout defaultLayoutSpec ["a b c"]
            fmap (map cellToken) r1 `shouldBe` fmap (map cellToken) r2

        it "drops emptyToken cells from output" $ do
            let lspec = defaultLayoutSpec { emptyToken = Just "___" }
                rows = ["aaa ___ bbb"]
            fmap (map cellToken) (concatHexRows $ parseHexLayout lspec rows) `shouldBe` Right ["aaa", "bbb"]

        it "does not drop tokens that merely contain the empty sentinel as a substring" $ do
            let lspec = defaultLayoutSpec { emptyToken = Just "__" }
                rows = ["___ aaa"]
            fmap (map cellToken) (concatHexRows $ parseHexLayout lspec rows) `shouldBe` Right ["___", "aaa"]

        it "accumulates RowLengthMismatch errors when strictWidth is set" $ do
            let lspec = defaultLayoutSpec { strictWidth = Just 3 }
                rows = ["a b c", "a b", "a b c d"]
            parseHexLayout lspec rows `shouldBe`
                Left [ RowLengthMismatch { leRow = 1, leExpected = 3, leActual = 2 }
                     , RowLengthMismatch { leRow = 2, leExpected = 3, leActual = 4 }
                     ]

        it "accepts rows when strictWidth matches every row" $ do
            let lspec = defaultLayoutSpec { strictWidth = Just 2 }
            fmap (length . concat) (parseHexLayout lspec ["a b", "c d"]) `shouldBe` Right 4

        it "reproduces the authored example from the design notes" $ do
            -- Visual layout (indentation cosmetic only):
            --   cd1   a6b   pea   rl
            --       a4b   cd1   nex
            --   v2r   cd1   joe   a2b
            let rows =
                    [ "cd1   a6b   pea   rl"
                    , "    a4b   cd1   nex"
                    , "v2r   cd1   joe   a2b"
                    ]
            fmap (map (\c -> (c.cellToken, c.cellQ, c.cellR))) (concatHexRows $ parseHexLayout defaultLayoutSpec rows)
                `shouldBe` Right
                    [ ("cd1",  0, 0), ("a6b", 1, 0), ("pea", 2, 0), ("rl",  3, 0)
                    , ("a4b",  0, 1), ("cd1", 1, 1), ("nex", 2, 1)
                    , ("v2r", -1, 2), ("cd1", 0, 2), ("joe", 1, 2), ("a2b", 2, 2)
                    ]

    describe "makeDictionary" $ do

        it "accepts a list with unique keys" $
            makeDictionary [("a", 1 :: Int), ("b", 2), ("c", 3)]
                `shouldBe` Right (Map.fromList [("a", 1), ("b", 2), ("c", 3)])

        it "returns DuplicateDictionaryKey for every repeated key" $
            makeDictionary [("a", 1 :: Int), ("b", 2), ("a", 3)]
                `shouldBe` Left [DuplicateDictionaryKey "a"]

        it "reports all duplicate keys, not just the first" $ do
            let result = makeDictionary [("x", 1 :: Int), ("y", 2), ("x", 3), ("y", 4)]
            result `shouldSatisfy` isLeft
            -- Both x and y must appear as errors
            case result of
                Left errs -> map (.leToken) errs `shouldMatchList` ["x", "y"]
                Right _   -> expectationFailure "expected Left"

    describe "mapHexLayout" $ do

        it "maps cells to descriptors using the dictionary" $ do
            let dict  = Map.fromList [("a", 10 :: Int), ("b", 20)]
                cells = parseCells defaultLayoutSpec ["a b"]
            fmap (map snd) (mapHexLayout dict cells) `shouldBe` Right [10, 20]

        it "accumulates UnknownToken errors for missing tokens" $ do
            let dict  = Map.fromList [("a", () :: ())]
                cells = parseCells defaultLayoutSpec ["a z"]
            mapHexLayout dict cells `shouldBe`
                Left [UnknownToken { leRow = 0, leCol = 1, leToken = "z" }]

        it "reports all unknown tokens, not only the first" $ do
            let dict  = Map.empty :: LayoutDictionary ()
                cells = parseCells defaultLayoutSpec ["x y z"]
            mapHexLayout dict cells `shouldSatisfy` isLeft
            case mapHexLayout dict cells of
                Left errs -> length errs `shouldBe` 3
                Right _   -> expectationFailure "expected Left"

    describe "toCubeCoordinates" $ do

        it "converts parsed cells to CubeCoordinate pairs" $ do
            let cells = parseCells defaultLayoutSpec ["a b"]
                dict  = Map.fromList [("a", 'A'), ("b", 'B')]
            fmap toCubeCoordinates (mapHexLayout dict cells)
                `shouldBe` Right
                    [ (mkCubeCoordinate 0 0, 'A')
                    , (mkCubeCoordinate 1 0, 'B')
                    ]

    describe "hexLayoutToCoordinates" $ do

        it "is equivalent to parse then map then extract" $ do
            let dict = Map.fromList [("aa", True), ("bb", False)]
                rows = ["aa bb", "bb aa"]
            hexLayoutToCoordinates defaultLayoutSpec dict rows
                `shouldBe` Right
                    [ (mkCubeCoordinate 0 0, True)
                    , (mkCubeCoordinate 1 0, False)
                    , (mkCubeCoordinate 0 1, False)
                    , (mkCubeCoordinate 1 1, True)
                    ]

    describe "findRowCellMatches" $ do

        it "finds all non-overlapping run matches in a row" $ do
            let row = (0, parseCells defaultLayoutSpec ["a b a b c"])
            let result = findRowCellMatches (Nothing, (["a", "b"], "hit" :: Text)) row
            fmap (map (\(i, (cells, p)) -> (i, map cellToken cells, p))) result
                `shouldBe` Right [(0, ["a", "b"], "hit"), (2, ["a", "b"], "hit")]

        it "returns SearchCountWrongError when expected row count does not match" $ do
            let row = (0, parseCells defaultLayoutSpec ["a b a"])
            findRowCellMatches (Just 1, (["a"], () :: ())) row
                `shouldBe` Left [SearchCountWrongError { cpExpected = 1, cpActual = 2 }]

        it "returns OverlappingCellSearch when row run matches overlap" $ do
            let row = (3, parseCells defaultLayoutSpec ["a a a"])
            findRowCellMatches (Nothing, (["a", "a"], () :: ())) row
                `shouldBe` Left [OverlappingCellSearch { leRow = 3, leCol = 1 }]

    describe "findRowMixedCellMatches" $ do

        it "finds mixed run matches and keeps row order by start column" $ do
            let row = (0, parseCells defaultLayoutSpec ["a b c d"])
            let searches = [(Nothing, (["c"], "C" :: Text)), (Nothing, (["a"], "A" :: Text))]
            let result = findRowMixedCellMatches searches row
            fmap (map (\(i, (cells, p)) -> (i, map cellToken cells, p))) result
                `shouldBe` Right [(0, ["a"], "A"), (2, ["c"], "C")]

        it "returns OverlappingCellSearch when different searches overlap" $ do
            let row = (1, parseCells defaultLayoutSpec ["a b c"])
            let searches =
                    [ (Nothing, (["a", "b"], () :: ()))
                    , (Nothing, (["b", "c"], () :: ()))
                    ]
            findRowMixedCellMatches searches row
                `shouldBe` Left [OverlappingCellSearch { leRow = 1, leCol = 1 }]

    describe "findCellMatches" $ do

        it "finds matches across rows" $ do
            let rows = parseRows defaultLayoutSpec ["x a", "a x"]
            let result = findCellMatches (Just 2) (["a"], "match" :: Text) rows
            fmap (map (first (map cellToken))) result
                `shouldBe` Right [(["a"], "match"), (["a"], "match")]

        it "returns SearchCountWrongError when total count does not match" $ do
            let rows = parseRows defaultLayoutSpec ["x a", "a x"]
            findCellMatches (Just 1) (["a"], () :: ()) rows
                `shouldBe` Left [SearchCountWrongError { cpExpected = 1, cpActual = 2 }]

    describe "findAllCellMatches" $ do

        it "collects matches for all search patterns across rows" $ do
            let rows = parseRows defaultLayoutSpec ["a b", "b a"]
            let searches = [(["a"], "A" :: Text), (["b"], "B" :: Text)]
            let result = findAllCellMatches searches rows
            fmap (map (first (map cellToken))) result
                `shouldBe` Right [(["a"], "A"), (["b"], "B"), (["b"], "B"), (["a"], "A")]

        it "returns overlap errors discovered across mixed searches" $ do
            let rows = parseRows defaultLayoutSpec ["a b c"]
            let searches = [(["a", "b"], () :: ()), (["b", "c"], () :: ())]
            findAllCellMatches searches rows
                `shouldBe` Left [OverlappingCellSearch { leRow = 0, leCol = 1 }]

    describe "toPatch" $ do

        it "maps before and after patch cells into coordinate descriptor pairs" $ do
            let cells = parseCells defaultLayoutSpec ["a b"]
                patchFn [a, b] = [a { cellToken = "x" }, b { cellToken = "y" }]
                patchFn _ = []
                dict = Map.fromList [("a", 1 :: Int), ("b", 2), ("x", 10), ("y", 20)]
            toPatch dict [(cells, patchFn)]
                `shouldBe` Right
                    [ ((mkCubeCoordinate 0 0, 1), (mkCubeCoordinate 0 0, 10))
                    , ((mkCubeCoordinate 1 0, 2), (mkCubeCoordinate 1 0, 20))
                    ]

        it "hoists map errors as FailedParseError" $ do
            let cells = parseCells defaultLayoutSpec ["a"]
                patchFn [a] = [a { cellToken = "z" }]
                patchFn _ = []
                dict = Map.fromList [("a", 1 :: Int)]
            toPatch dict [(cells, patchFn)]
                `shouldBe` Left
                    [ FailedParseError
                        (UnknownToken { leRow = 0, leCol = 0, leToken = "z" })
                    ]

    describe "hexLayoutPatchToCoordinates" $ do

        it "parses, matches, patches, and maps coordinates end-to-end" $ do
            let rows = ["a b", "b a"]
                dict = Map.fromList [("a", 1 :: Int), ("b", 2), ("x", 9)]
                patchA [a] = [a { cellToken = "x" }]
                patchA _ = []
                searches = [(["a"], patchA)]
            hexLayoutPatchToCoordinates defaultLayoutSpec dict rows searches
                `shouldBe` Right
                    [ ((mkCubeCoordinate 0 0, 1), (mkCubeCoordinate 0 0, 9))
                    , ((mkCubeCoordinate 1 1, 1), (mkCubeCoordinate 1 1, 9))
                    ]

        it "hoists layout parse failures to FailedParseError" $ do
            let dict = Map.fromList [("a", 1 :: Int)]
                searches = [(["a"], id)]
            hexLayoutPatchToCoordinates defaultLayoutSpec dict [] searches
                `shouldBe` Left [FailedParseError EmptyLayout]

    describe "coordinatesToVoxelBlock" $ do

        it "builds a dense rectangular block from sparse coordinates" $ do
            let input =
                    [ (mkCubeCoordinate 2 4, NamedVoxel "a")
                    , (mkCubeCoordinate 3 5, NamedVoxel "b")
                    ]
                (firstColumn, columns) = coordinatesToVoxelBlock input
            firstColumn `shouldBe` 4
            map fst columns `shouldBe` [2, 2]
            map (map voxelOptionLabel . snd) columns
                `shouldBe` [["a", "_"], ["_", "b"]]

        it "uses last-entry-wins semantics for duplicate coordinates" $ do
            let input =
                    [ (mkCubeCoordinate 0 0, NamedVoxel "old")
                    , (mkCubeCoordinate 0 0, NamedVoxel "new")
                    ]
                (firstColumn, columns) = coordinatesToVoxelBlock input
            firstColumn `shouldBe` 0
            map fst columns `shouldBe` [0]
            map (map voxelOptionLabel . snd) columns
                `shouldBe` [["new"]]

        it "returns an empty block for empty input" $ do
            let (firstColumn, columns) = coordinatesToVoxelBlock []
            firstColumn `shouldBe` 0
            null columns `shouldBe` True

    describe "64 by 48 scale" $ do

        it "parses 3072 cells without error" $ do
            let tok  = "aaa" :: Text
                row  = unwords (replicate 64 tok)
                rows = replicate 48 row
                dict = Map.fromList [(tok, () :: ())]
            fmap length (hexLayoutToCoordinates defaultLayoutSpec dict rows)
                `shouldBe` Right 3072

        it "top-left cell is at axial (0, 0)" $ do
            let tok  = "aaa" :: Text
                row  = unwords (replicate 64 tok)
                rows = replicate 48 row
                dict = Map.fromList [(tok, () :: ())]
            case hexLayoutToCoordinates defaultLayoutSpec dict rows of
                Left  errs  -> expectationFailure ("parse errors: " <> show errs)
                Right ((coord, _) : _) -> coord `shouldBe` mkCubeCoordinate 0 0
                Right [] -> expectationFailure "expected non-empty result"

        it "bottom-right cell of row 47 has r=47 and correct q" $ do
            -- row 47 (even), col 63: r=47, q = 63 - 47`div`2 = 63 - 23 = 40
            let tok  = "aaa" :: Text
                row  = unwords (replicate 64 tok)
                rows = replicate 48 row
                dict = Map.fromList [(tok, () :: ())]
            case hexLayoutToCoordinates defaultLayoutSpec dict rows of
                Left  errs  -> expectationFailure ("parse errors: " <> show errs)
                Right pairs ->
                    case reverse pairs of
                        ((coord, _) : _) -> coord `shouldBe` mkCubeCoordinate 40 47
                        []               -> expectationFailure "expected non-empty result"

        it "strictWidth=64 accepts all 48 rows without error" $ do
            let lspec = defaultLayoutSpec { strictWidth = Just 64 }
                tok  = "aaa" :: Text
                row  = unwords (replicate 64 tok)
                rows = replicate 48 row
            fmap (length . concat) (parseHexLayout lspec rows) `shouldBe` Right 3072

-- ---------------------------------------------------------------------------
-- Helpers (local to this spec)
-- ---------------------------------------------------------------------------

-- | Run a single cell through 'parseHexLayout' and return it.  Dies loudly
--   if parsing fails — only used in coordinate-math tests where the layout is
--   always valid.
parseCell :: LayoutSpec -> Int -> Int -> Text -> ParsedHexCell
parseCell layoutSpec rowIdx colIdx tok =
    let row  = unwords (replicate (colIdx + 1) "x")
        rows = replicate (rowIdx + 1) row
    in  case concatHexRows $ parseHexLayout layoutSpec rows of
            Left  errs  -> error ("parseCell: unexpected errors: " <> show errs)
            Right cells ->
                case filter (\c -> c.cellRow == rowIdx && c.cellCol == colIdx) cells of
                    (c : _) -> c { cellToken = tok }
                    []      -> error ("parseCell: cell not found at row=" <> show rowIdx <> " col=" <> show colIdx)

-- | Extract (q, r) from a 'ParsedHexCell'.
cellCoords :: ParsedHexCell -> (Int, Int)
cellCoords c = (c.cellQ, c.cellR)

-- | Parse rows with 'defaultLayoutSpec', failing loudly on any error.
parseCells :: LayoutSpec -> [Text] -> [ParsedHexCell]
parseCells layoutSpec rows =
    case concatHexRows $ parseHexLayout layoutSpec rows of
        Left  errs  -> error ("parseCells: unexpected errors: " <> show errs)
        Right cells -> cells

parseRows :: LayoutSpec -> [Text] -> [[ParsedHexCell]]
parseRows layoutSpec rows =
    case parseHexLayout layoutSpec rows of
        Left  errs     -> error ("parseRows: unexpected errors: " <> show errs)
        Right cellRows -> cellRows

-- | Convenience accessor (avoids importing Data.Text in test body).
cellToken :: ParsedHexCell -> Text
cellToken = (.cellToken)

voxelOptionLabel :: VoxelOption -> Text
voxelOptionLabel NoVoxel = "_"
voxelOptionLabel (NamedVoxel name) = name
voxelOptionLabel (JustVoxel _) = "just"
