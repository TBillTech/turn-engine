module Game.CursedTreasure.API
    ( getGameSetupPlayers
    , createNewGame
    , enumerateActivePlayerOptions
    , makeMove
    , heuristicHint
    , summary
    -- Below are exported for use in testing, the above is the set of API functions.
    , mkCensoredGameState
    , distanceSet
    , fillHexOcean
    , getConnectedSets
    , isValidTerrainBoard
    , createBoard
    , nextTurn
    , passTurnOption
    , pickupAmuletCase
    , useAmuletCase
    , raiseTreasureCase
    , raisingTreasureChoiceCase
    , raisingTreasureViewCase
    , enumeratePossibleJeepMoves
    , isRaisingTreasure
    , matchObject
    , matchClueCard
    , applyClue
    , enumeratePossibleCluePlays
    , enumeratePlayerOptions
    , makeMoveDirect
    , makeMoveSummary
    )
where

import Lens.Micro.Platform ((%~), Lens', lens, Traversal')
import qualified Data.Text as Text
import Relude.Extra (bimapF, maximum1, minimum1)
import Relude.Extra.Map (insert, keys, member, notMember, toPairs, alter)
import System.Random (mkStdGen, uniformR, RandomGen, uniformShuffleList, splitGen, StdGen)
import qualified Data.Map.Strict as Map (elemAt, elems, empty,
    lookup, union, filter, filterWithKey, restrictKeys, withoutKeys, delete)
import qualified Data.Set as Set (delete, difference, intersection,
    union, empty, filter, elemAt, insert, size,
    unions)

import Game.CursedTreasure.Types
    ( CensoredGameState
    , ClueCard (..)
    , ClueObject (..)
    , ClueColor (..)
    , ClueBoard
    , Feature (..)
    , GameState (..)
    , PlayerDescription (..)
    , PlayerId
    , PlayerState (..)
    , RaisingTreasureState (..)
    , TerrainToken (..)
    , TreasureCard (..)
    , PlayerMove
    , TerrainHex (..)
    , allPlayerColors
    , allClueColors
    , Score (..)
    , HexMap
    , HexSet
    , HexBoard
    , allFeatures
    , Deck
    , TreasureBoard
    , PlayerMove (..)
    , GameMode (..)
    , allPlayerIds
    , mkCensoredGameState
    )
import Game.Core.Primitives
    ( adjacentCubeCoordinates
    , mkCubeCoordinate
    , cubeCoordinateDistance
    , cubeCoordinateDistFloor
    , toPair
    , isUnitCubeDist
    , toHourHand
    , CubeCoordinateTokens (CubeCoordinateTokens)
    , CubeCoordinate
    )

-- | Builds the default player roster by pairing each legal color with a numbered player.
getGameSetupPlayers :: [PlayerDescription]
getGameSetupPlayers = zipWith mkPlayerDescription allPlayerIds allPlayerColors
    where
        mkPlayerDescription playerId c =
            PlayerDescription
                { playerId = playerId
                , playerName = "Player " <> show playerId
                , playerAI = "Unassigned"
                , playerColor = c
                }

-- | Returns the full game state together with a censored view for every player.
censorGame :: GameState -> (GameState, [CensoredGameState])
censorGame = censor
    where   playerIds s = map (\pd -> pd.player.playerId) s.players
            censor s = (s, map (mkCensoredGameState s) $ playerIds s)


-- | Creates a fresh game from player descriptions and a deterministic random seed.
createNewGame :: [PlayerDescription] -> Int -> (GameState, [CensoredGameState])
createNewGame players randomSeed = censorGame newGameState
    where newGameState = createNewGameState players randomSeed

    -- | Initializes a player state with no cards, treasures, or remaining actions.
createNewPlayer :: PlayerDescription -> PlayerState
createNewPlayer pd = initPlayerState
    where initPlayerState = PlayerState
            { player = pd
            , clues = []
            , amulets = 0
            , foundTreasures = []
            , availableJeepMoves = 0
            , availableCluePlays = 0
            , availableRemoveMarkers = 0
            , availablePickupAmulet = 0
            , availableClueCardExchange = 0
            , score = CurrentScore 0
            , viewingTreasures = [] }

-- | Creates the positive and negative distance-based clues for a clue object.
mkWithinStepsOfClues :: ClueObject -> [ClueCard]
mkWithinStepsOfClues co = [WithinStepsOf 1 co
                          ,WithinStepsOf 2 co
                          ,NotWithinStepsOf 1 co
                          ,NotWithinStepsOf 2 co]

-- | Creates the direct presence and absence clues for a clue object.
mkIsOnClues :: ClueObject -> [ClueCard]
mkIsOnClues co = [IsOn co, IsNotOn co]

-- | Creates the full clue set for an object by combining distance and exact-match clues.
mkWithinAndOnClues :: ClueObject -> [ClueCard]
mkWithinAndOnClues co = mkWithinStepsOfClues co ++ mkIsOnClues co

-- | Expands a terrain feature into all clue cards that can reference it.
mkFeatureClues :: Feature -> [ClueCard]
mkFeatureClues Ocean = mkWithinStepsOfClues $ FeatureClue False Ocean
mkFeatureClues f = mkWithinAndOnClues (FeatureClue True f) ++
                   mkWithinAndOnClues (FeatureClue False f)

-- | Expands a terrain token into its distance-based clue cards.
mkTokenClues :: TerrainToken -> [ClueCard]
mkTokenClues t = mkWithinStepsOfClues $ TokenClue t

-- | The complete clue deck before shuffling.
allClues :: [ClueCard]
allClues = featureClues ++ terrainClues ++ statueClues
    where featureClues = concatMap mkFeatureClues allFeatures
          terrainClues = concatMap mkTokenClues [PalmTree, Hut]
          statueClues = mkWithinStepsOfClues StatueClue

-- | The complete treasure deck before stacking curses near the bottom.
allTreasures :: [TreasureCard]
allTreasures = map Treasure $ replicate 3 6 ++
                              replicate 6 5 ++
                              replicate 11 4 ++
                              replicate 9 3 ++
                              replicate 10 2

-- | Shuffles treasures so the first twelve draws are guaranteed non-curse cards.
stackTreasure :: RandomGen g => g -> ([TreasureCard], g)
stackTreasure g = (twelve ++ bottom, g'')
    where (randomTreasures, g') = uniformShuffleList allTreasures g
          (twelve, rest) = splitAt 12 randomTreasures
          (bottom, g'') = uniformShuffleList (Curse:Curse:rest) g'

-- | Returns all spaces within the requested expansion distance from the seed set.
distanceSet :: Int -> HexSet -> HexSet
distanceSet dist existingIndexes = Set.unions $ getDistanceSets dist [existingIndexes]
    where
        getDistanceSets 0 sets = sets
        getDistanceSets _ [] = []
        getDistanceSets reps (lastSet:rest) = getDistanceSets (reps-1) $ nextSet:lastSet:rest
            where   nextSet = getBoundarySet (const True) lastSet


-- | Places one terrain token while trying to keep copies of that token separated.
addToken :: RandomGen g => Int -> TerrainToken -> (HexMap, g) -> (HexMap, g)
addToken dist t (b, g)
    | null possibleSet = (b, g)
    | dist > 0 && null consideredSet = addToken (dist-1) t (b, g)
    | otherwise = (alter addIt candidateI b, g')
    where
        (i, g') = uniformR (0, length candidateSet - 1) g
        candidateSet = if dist > 0 && not (null consideredSet) then consideredSet else possibleSet
        candidateI = Set.elemAt i candidateSet
        possibleB = Map.filter possible b
        possibleSet = fromList . keys $ possibleB
        consideredSet = possibleSet `Set.difference` distSet
        distSet = distanceSet dist existingIndexes
        existingIndexes = fromList . keys $ Map.filter hasT b
        hasT (TerrainHex _ _ ts) = toZeroStatue t `elem` map toZeroStatue ts
        toZeroStatue (Statue _) = Statue $ toHourHand (0 :: Int)
        toZeroStatue th = th
        possible (TerrainHex _ f ts)
            | (not . null) ts = False
            | f == Ocean = False
            | f == Lagoon = False
            | f == Mountain = False
            | t == PalmTree && f == Jungle = False
            | otherwise = True
        addIt (Just (TerrainHex isL f ts)) = Just $ TerrainHex isL f $ t : ts
        addIt Nothing = Just $ TerrainHex False Meadow [t]

-- | Repeats 'addToken' the requested number of times.
addTokens :: RandomGen g => Int -> TerrainToken -> (HexMap, g) -> (HexMap, g)
addTokens 0 _ bG = bG
addTokens n t bG = addTokens (n-1) t $ addToken 4 t bG

-- | Finds all adjacent spaces around a set, excluding the set itself.
getBoundarySet :: (CubeCoordinate Int -> Bool) -> HexSet -> HexSet
getBoundarySet filt ts = Set.filter filt $ superKeys `Set.difference` ts
    where superKeys = fromList $ concatMap adjacentCubeCoordinates ts

-- | Finds all empty boundary spaces adjacent to hexes matching the predicate.
getBoundary :: (CubeCoordinate Int -> TerrainHex -> Bool) -> HexMap -> HexSet
getBoundary filt m = getBoundarySet setFilter keySet
    where setFilter t = t `notMember` m
          keySet = fromList $ map fst (filter (uncurry filt) $ toPairs m)

-- | Matches any terrain hex that is not ocean.
isNonOceanTerrain :: CubeCoordinate Int -> TerrainHex -> Bool
isNonOceanTerrain _ (TerrainHex _ Ocean _) = False
isNonOceanTerrain _ _ = True

-- | Wraps the generated island with two rings of ocean hexes.
fillHexOcean :: RandomGen g => (HexMap, g) -> (HexMap, g)
fillHexOcean (m, g) = (Map.union withFirst secondMap, g)
    where firstBoundary = getBoundary isNonOceanTerrain m
          withFirst = foldr (\k m' -> insert k (TerrainHex True Ocean []) m') m firstBoundary
          secondBoundary = toList $ getBoundary (const . const True) withFirst
          secondMap = fromList $ map (, TerrainHex True Ocean []) secondBoundary

-- | Flood-fills connected neighbors starting from the provided key list. 
-- First argument is the current set so far, Second argument is list of keys which will be added next,
-- and third argument is the set of keys which might be added in later steps
-- The return value is the updated set, the new list of neighboring keys to the last list of keys, 
-- and the unadded keys minus the second argument.
splitNeighbors :: HexSet -> [CubeCoordinate Int] -> HexSet
    -> (HexSet, [CubeCoordinate Int], HexSet)
splitNeighbors c ks s | null keyIntersection = (c', [], s')
                      | otherwise = splitNeighbors c' (toList keyIntersection) s'
    where s' = Set.difference s keyIntersection
          c' = c `Set.union` fromList ks
          keyCandidates = fromList $ concatMap adjacentCubeCoordinates ks
          keyIntersection = Set.intersection keyCandidates s

-- | Splits a map into one connected component and the remaining disconnected hexes.
getSimplyConnectedSet :: HexMap -> (HexMap, HexMap)
getSimplyConnectedSet m | null m = (m, m)
                        | otherwise = (connectedMap, remainderMap)
    where th0 = Map.elemAt 0 m
          keySet = fromList (keys m)
          (connectedSet, _, _) = splitNeighbors Set.empty [fst th0] keySet
          connectedMap = Map.restrictKeys m connectedSet
          remainderMap = Map.withoutKeys m connectedSet


-- | Partitions a terrain map into all connected components.
getSimplyConnectedSets :: HexMap -> [HexMap]
getSimplyConnectedSets m | null m = []
                         | otherwise = found:getSimplyConnectedSets m'
    where (found, m') = getSimplyConnectedSet m

-- | Groups connected components for a single terrain feature, sorted by largest first.
getFeatureConnectedSets :: HexMap -> Feature -> (Feature, [HexMap])
getFeatureConnectedSets m f = (f, sortedMaps)
    where unsortedMaps = getSimplyConnectedSets $ Map.filter (\(TerrainHex _ f' _) -> f' == f) m
          sortedMaps = sortBy (\m1 m2 -> length m2 `compare` length m1) unsortedMaps

-- | Groups connected components for every terrain feature on the board, keeping Ocean _first_.
getConnectedSets :: HexMap -> [(Feature, [HexMap])]
getConnectedSets m = map (getFeatureConnectedSets m) allFeatures

-- | Checks the terrain layout invariant used by board generation. Relies on the
-- | results from getConnectedSets being in order from largest to smallest.
-- | This function requires at least three territories for each non-ocean feature
-- | and forbids territories of size 1 or less except for Lagoon, which is
-- | allowed up to 2 singleton territories.
isValidTerrainBoard :: HexMap -> Bool
isValidTerrainBoard m = all isValid sets && hasAllTerrains
    where   sets = getConnectedSets m
            hasLargest (Ocean, _) = True
            hasLargest (_, [_]) = True
            hasLargest (_, fg0:fg1:_) = length fg0 > length fg1
            hasLargest _ = False
            hasEnough (Ocean, fs) = not (null fs)
            hasEnough (_, fs) = 3 <= length fs
            isValid feature = hasLargest feature && noDegenerates feature && hasEnough feature
            noDegenerates (Lagoon, fs) = not (any null fs) && 2 >= length (singles fs)
            noDegenerates (_, fs) = not (any ((1 >=) . length) fs)
            singles = filter ((1 ==) . length)
            hasAllTerrains =
                length presentFeatures == length allFeatures
                    && all (`elem` presentFeatures) allFeatures
                where
                    presentFeatures = map fst sets


-- | Repairs disconnected ocean regions by converting isolated oceans into lagoons when possible.
fillInTerrain :: RandomGen g => (HexMap, g) -> (Bool, (HexMap, g))
fillInTerrain (m, g) | contiguous = (True, (m, g))
                     | solved = (True, (solution, g))
                     | otherwise = (False, (m, g))
    where contiguous = isOcean && length oceans == 1
          solved = isOcean && isValidTerrainBoard solution
          solution = toLagoon isolated m
          connectedSets = getConnectedSets m
          mOcean = fst <$> uncons (filter ((/= Ocean) . fst) connectedSets)
          isOcean = maybe False ((== Ocean) . fst) mOcean
          oceans = maybe [Map.empty] snd mOcean
          isolated = map (convertToLagoon <$>) $ drop 1 oceans
          convertToLagoon (TerrainHex isL _ ts) = TerrainHex isL Lagoon ts
          toLagoon [] m' = m'
          toLagoon (iso:isos) m' = toLagoon isos $ Map.union iso m'

-- | Seeds new terrain clusters near existing land while preserving spacing rules.
-- | This function is expected to be called on Terrain Logoon, then River, then Mountain.
-- | When Mountain is called, it is expected that originOccupied is false, so that the first
-- | Mountain terrain seed will be (0,0) 
addTerrainSeeds :: RandomGen g => Int -> TerrainHex -> (HexMap, g)
    -> ([CubeCoordinate Int], (HexMap, g))
addTerrainSeeds count tHex (m, g) = (fst seedsG, first (Map.union m) mapG)
    where   boundary = getBoundary isNonOceanTerrain m
            oceanBound = m `Map.restrictKeys` boundary
            available = Set.difference boundary $ fromList $ keys oceanBound
            shuffledG = uniformShuffleList (toList available) g
            originOccupied = mkCubeCoordinate 0 0 `member` m
            waterHex = case tHex of (TerrainHex _ f _) -> f `elem` [Ocean, Lagoon, River]
            optionsG    | originOccupied || waterHex = shuffledG
                        | otherwise = first (mkCubeCoordinate 0 0:) shuffledG
            mapG = first (fromList . map (, tHex)) seedsG
            seedsG  | length (fst shuffledG) < count = first initialize shuffledG
                    | tHex == TerrainHex False River [] = sift $ first riverFilter shuffledG
                    | otherwise = sift $ first adjFilter optionsG
            initialize sds  | length sds >= count = take count sds
                            | otherwise = initialize $ newI:sds
                where   newI = findClear sds (shifts $ length sds) (lastIndex sds)
                        lastIndex [] = mkCubeCoordinate 0 0
                        lastIndex (t:_) = t
                        shifts i | i `mod` 3 == 0 = ((+3), (+0))
                                 | i `mod` 3 == 1 = ((+ (-4)), (+2))
                                 | otherwise = ((+0), (+ (-4)))
            findClear sds (app0, app1) coord
                = findClear' sds (app0, app1) (mkCubeCoordinate (app0 i) (app1 j))
                where (i, j) = toPair coord
            findClear' sds (app0, app1) t
                | t `member` m = findClear sds (app0, app1) t
                | t `elem` sds = findClear sds (app0, app1) t
                | otherwise = t

            adjnt = isUnitCubeDist
            adjFilter [] = []
            adjFilter (t0:rest) = t0:filter (not . adjnt t0) (adjFilter rest)
            sift (sds, rg)  | length sds >= count = (take count sds, rg)
                            | otherwise = sift (adjFilter shuffled', rg')
                where (shuffled', rg') = uniformShuffleList (fst shuffledG) rg

            riverFilter = adjFilter . filter distFilter
                where distFilter = (4 <=) . cubeCoordinateDistance (mkCubeCoordinate 0 0)

-- | Controls how seeded terrain expands during board generation.
data GrowthRule = RandomGrowth | RiverGrowth | CoastalGrowth

-- | Seeds and then grows one feature across multiple independent clusters.
-- | Note that addTerrainSeeds when tHex is Mountain is expected to set the first Mountain seed to (0,0)
growTerrainSeeds :: RandomGen g => (GrowthRule, [Int]) -> TerrainHex -> (HexMap, g) -> (HexMap, g)
growTerrainSeeds (rule, sizes) tHex mg = grownG
    where   (seeds, withSeedsG) = addTerrainSeeds (length sizes) tHex mg
            seedSets = zipWith (\size seed -> (size, seed, one seed)) sizes seeds
            grownG = foldr (growTerrainSeed seedSets rule tHex) withSeedsG seedSets

-- | Expands one seeded cluster according to the requested growth rule. 
-- | Growing no seeds returns a likewise empty map.
-- | For all of these growTerrainSeed functions, the goal is to add one hex which touches the HexSet grown
-- | so far, decrementing i, thus creating a territory hex by hex. For all of these growTerrainSeed functions,
-- | the following restriction is imposed on the possible new hex locations:
-- | 1) It must first of all touch one of the terrain hexes in the currently growing terrain HexSet
-- | 2) It must not be overwriting another hex already in the map assigned to another territory
-- | 3) It must not be touching territories grown from other seeds of the same territory kind; this is to ensure
-- |    that when growing seeds, the number of desired territories defined by the sizes list in growTerrainSeeds
-- |    does not decrease due to merges of the same territory kind.
-- | 4) In the event that the seed itself is already touching another terrain of the same kind, then repair the seed
-- |    by moving it to the bordering hex of the seed furthest from the origin, one step at a time, until
-- |    the seed is now on an unoccupied hex AND not adjacent to another terrain of the same kind. 
-- | In addition the growth rules each have some extra additional constraints on how the next hex is chosen:
-- | For RandomGrowth we want to try to keep the terrain more circular, but also grow towards the center of the board at (0,0)
-- | RandomGrowth I) If there are any candidate hexes which lie closer to (0,0) than the originalSeed, then
-- |                 measure the sum of the distance between this hex and the (0,0) (distToO), and measure
-- |                 the distnace from this hex and the original seed location (distToSeed). Also measure
-- |                 the distance from the original seed to the (0,0) (seedDistTo0). 
-- |                 Filter candidates closer to (0,0) than the seed by this test:
-- |                 closerFilt = distTo0 + distToSeed <= 2 + seedDistTo0.
-- |                 If any candidate hexes survive this filter, then always choose from the candidates that
-- |                 are closest to the (0,0), causing the terrain to grow towards the (0,0) hex if possible.
-- | RandomGrowth II) If the seed was actually at (0,0), then ignore RandomGrowth rules I and III, and simply choose
-- |                 an avaialble hex from the set closest to (0,0)
-- | RandomGrowth II) If the seed was actually at (0,0), then when i reaches 0, an additional constraint must be met:
-- |                 All the hexes belonging to the grown territory must either be completely surrounded, OR must be 
-- |                 adjacent to at least 2 unoccupied hexes. This is to prevent the Lagoons from being merged together
-- |                 when isolated ocean tiles are converted to lagoons at a later stage of the process. If there is
-- |                 a hex that fails this rule, then a neighboring unoccupied hex must be converted to this terrain
-- |                 type and added to the territory. This must continue until all the hexes satisfy this rule.
-- | RandomGrowth III) If there are no hexes that satisfy RandomGrowth I, and the seed is not at (0,0), then
-- |                 choose one of the available hexes at random that is closest to the seed (minimum distToSeed).
-- | For RiverGrowth we want to try to keep the terrain more like a long chain, starting closer to (0,0) and flowing
-- | outward like a river does. However, we don't want a perfect raidal spike, we want some random meandering.
-- | RiverGrowth I) Find all hexes in the territory so far that are furthest from the (0,0) AND which have only 1 
-- |                neighboring hex in the territory so far. Consider as candidates any unoccupied hex 
-- |                that is adjacent to the furthest hexes found. Rule out any candidate hex that is closer
-- |                to (0,0) than the furthest hex distance.  
-- | RiverGrowth II) Also Rule out any candidate hex that is adjacent to more than 1 hex already in the growing 
-- |                territory.
-- | For CoastalGrowth we want the terrain to be like a long chain, but to circle around the island and fill in 
-- | gaps in the coastline if possible. So distance from the seed is not important.
-- | CoastalGrowth I) Find all unoccupied hexes that are adjacent to the growing terrain, and choose from the hexes
-- |                that are closest to (0,0). 
-- | CoastalGrowth II) To keep the "coast" from clumping too much, rule out any hex that would, if added to the 
-- |                territory, be adjacent to 3 or more other hexes on the same territory.

growTerrainSeed :: RandomGen g => [(Int, CubeCoordinate Int, HexSet)] -> GrowthRule -> TerrainHex -> (Int, CubeCoordinate Int, HexSet) -> (HexMap, g) -> (HexMap, g)
growTerrainSeed _ RandomGrowth _ (0, originalSeed, territory) mapG =
    finalizeRandomGrowth originalSeed territory mapG
growTerrainSeed seeds RandomGrowth tHex (i, originalSeed, territory) mapG =
    maybe repairedMapG continueGrowth nextChoice
    where
        (repairedSeed, repairedTerritory, repairedMapG) = repairSeed seeds tHex originalSeed territory mapG
        territoryBound = validGrowthCandidates tHex repairedTerritory repairedMapG
        nextChoice = chooseRandomGrowth repairedSeed repairedTerritory territoryBound repairedMapG
        continueGrowth (toAdd, g') =
            growTerrainSeed seeds RandomGrowth tHex (i - 1, repairedSeed, Set.insert toAdd repairedTerritory) mapG'
            where
                mapG' = (insert toAdd tHex $ fst repairedMapG, g')
growTerrainSeed _ RiverGrowth _ (-1, _, _) mapG = mapG -- Stopping at -1 is for the Terminal Ocean tile
growTerrainSeed seeds RiverGrowth tHex (i, originalSeed, territory) mapG =
    maybe repairedMapG continueGrowth nextChoice
    where
        (repairedSeed, repairedTerritory, repairedMapG) = repairSeed seeds tHex originalSeed territory mapG
        territoryBound = riverGrowthCandidates tHex repairedTerritory repairedMapG
        nextChoice = chooseUniformSet territoryBound repairedMapG
        continueGrowth (toAdd, g') =
            growTerrainSeed seeds RiverGrowth tHex (i - 1, repairedSeed, Set.insert toAdd repairedTerritory) mapG'
            where
                usedTHex | i == 0 = TerrainHex True Ocean []
                         | otherwise = tHex
                mapG' = (insert toAdd usedTHex $ fst repairedMapG, g')
growTerrainSeed _ CoastalGrowth _ (0, _, _) mapG = mapG
growTerrainSeed seeds CoastalGrowth tHex (i, originalSeed, territory) mapG =
    maybe repairedMapG continueGrowth nextChoice
    where
        (repairedSeed, repairedTerritory, repairedMapG) = repairSeed seeds tHex originalSeed territory mapG
        territoryBound = coastalGrowthCandidates tHex repairedTerritory repairedMapG
        nextChoice = chooseUniformSet territoryBound repairedMapG
        continueGrowth (toAdd, g') =
            growTerrainSeed seeds CoastalGrowth tHex (i - 1, repairedSeed, Set.insert toAdd repairedTerritory) mapG'
            where
                mapG' = (insert toAdd tHex $ fst repairedMapG, g')

originCoordinate :: CubeCoordinate Int
originCoordinate = mkCubeCoordinate 0 0

terrainFeature :: TerrainHex -> Feature
terrainFeature (TerrainHex _ feature _) = feature

sameFeatureOutsideTerritory :: TerrainHex -> HexSet -> HexMap -> CubeCoordinate Int -> Bool
sameFeatureOutsideTerritory tHex territory board coord = any matchesFeature neighbors
    where
        feature = terrainFeature tHex
        neighbors = filter (`notMember` territory) $ adjacentCubeCoordinates coord
        matchesFeature neighbor = maybe False ((== feature) . terrainFeature) (Map.lookup neighbor board)

territoryNeighborCount :: HexSet -> CubeCoordinate Int -> Int
territoryNeighborCount territory coord = length $ filter (`member` territory) (adjacentCubeCoordinates coord)

validGrowthCandidates :: TerrainHex -> HexSet -> (HexMap, g) -> HexSet
validGrowthCandidates tHex territory mapG =
    Set.filter isValidCandidate $ getBoundarySet (`notMember` board) territory
    where
        board = fst mapG
        isValidCandidate coord = not (sameFeatureOutsideTerritory tHex territory board coord)

repairSeed :: RandomGen g => [(Int, CubeCoordinate Int, HexSet)] -> TerrainHex -> CubeCoordinate Int -> HexSet -> (HexMap, g) -> (CubeCoordinate Int, HexSet, (HexMap, g))
repairSeed _ tHex originalSeed territory mapG
    | Set.size territory /= 1 = (originalSeed, territory, mapG)
    | not (sameFeatureOutsideTerritory tHex territory (fst mapG) originalSeed) = (originalSeed, territory, mapG)
    | otherwise = maybe (originalSeed, territory, mapG) moveSeed repairedSeed
    where
        board = fst mapG
        boardWithoutSeed =
            case Map.lookup originalSeed board of
                Just existingHex | existingHex == tHex -> Map.delete originalSeed board
                _ -> board
        repairedSeed = findRepairAtDistance 1
        moveSeed newSeed = (newSeed, one newSeed, (insert newSeed tHex boardWithoutSeed, snd mapG))
        findRepairAtDistance dist
            | null ringCandidates = findRepairAtDistance (dist + 1)
            | otherwise = Just $ furthestFromOrigin ringCandidates
            where
                ringCandidates = filter canUse $ toList currentRing
                currentRing = distanceSet dist (one originalSeed) `Set.difference` distanceSet (dist - 1) (one originalSeed)
        canUse coord = coord `notMember` boardWithoutSeed && not (sameFeatureOutsideTerritory tHex Set.empty boardWithoutSeed coord)

furthestFromOrigin :: [CubeCoordinate Int] -> CubeCoordinate Int
furthestFromOrigin [] = originCoordinate
furthestFromOrigin (coord:coords) = foldl' pickFurther coord coords
    where
        originDist = cubeCoordinateDistFloor originCoordinate
        pickFurther best candidate
            | originDist candidate > originDist best 
                = candidate
            | otherwise = best

closestToOriginSet :: HexSet -> HexSet
closestToOriginSet options
    | null options = Set.empty
    | otherwise = Set.filter ((== minDist) . cubeCoordinateDistFloor originCoordinate) options
    where
        minDist = minDistanceClass (map (cubeCoordinateDistFloor originCoordinate) (toList options))

closestToSeedSet :: CubeCoordinate Int -> HexSet -> HexSet
closestToSeedSet seed options
    | null options = Set.empty
    | otherwise = Set.filter ((== minDist) . cubeCoordinateDistFloor seed) options
    where
        minDist = minDistanceClass (map (cubeCoordinateDistFloor seed) (toList options))

minDistanceClass :: [Int] -> Int
minDistanceClass dists = maybe maxBound minimum1 $ nonEmpty dists  

chooseUniformSet :: RandomGen g => HexSet -> (HexMap, g) -> Maybe (CubeCoordinate Int, g)
chooseUniformSet options mapG
    | null options = Nothing
    | otherwise = Just (Set.elemAt rI options, g')
    where
        (rI, g') = uniformR (0, length options - 1) $ snd mapG

chooseRandomGrowth :: RandomGen g => CubeCoordinate Int -> HexSet -> HexSet -> (HexMap, g) -> Maybe (CubeCoordinate Int, g)
chooseRandomGrowth originalSeed _ options mapG
    | null options = Nothing
    | originalSeed == originCoordinate = chooseUniformSet (closestToOriginSet options) mapG
    | not (null preferred) = chooseUniformSet (closestToOriginSet preferred) mapG
    | otherwise = chooseUniformSet (closestToSeedSet originalSeed options) mapG
    where
        seedDistTo0 = cubeCoordinateDistFloor originCoordinate originalSeed
        preferred = Set.filter closerFilter options
        closerFilter coord =
            distTo0 < seedDistTo0 && distTo0 + distToSeed <= seedDistTo0 + 2
            where
                distTo0 = cubeCoordinateDistFloor originCoordinate coord
                distToSeed = cubeCoordinateDistFloor originalSeed coord

riverGrowthCandidates :: TerrainHex -> HexSet -> (HexMap, g) -> HexSet
riverGrowthCandidates tHex territory mapG =
    Set.filter keepsRiverChain $ fromList $ concatMap candidateNeighbors riverEnds
    where
        baseCandidates = validGrowthCandidates tHex territory mapG
        endpoint coord = territoryNeighborCount territory coord == 1
        territoryList = toList territory
        endpointHexes = filter endpoint territoryList
        endpointDistances = map (cubeCoordinateDistFloor originCoordinate) endpointHexes
        maxDist = case endpointDistances of
            [] -> 0
            dist:restDists -> foldl' max dist restDists
        riverEnds = filter ((== maxDist) . cubeCoordinateDistFloor originCoordinate) endpointHexes
        candidateNeighbors coord = filter (`member` baseCandidates) (adjacentCubeCoordinates coord)
        keepsRiverChain coord =
            cubeCoordinateDistFloor originCoordinate coord >= maxDist && territoryNeighborCount territory coord <= 1

coastalGrowthCandidates :: TerrainHex -> HexSet -> (HexMap, g) -> HexSet
coastalGrowthCandidates tHex territory mapG = closestToOriginSet filtered
    where
        filtered = Set.filter keepCoastEnd $ validGrowthCandidates tHex territory mapG
        keepCoastEnd coord = territoryNeighborCount territory coord < 3

finalizeRandomGrowth :: RandomGen g => CubeCoordinate Int -> HexSet -> (HexMap, g) -> (HexMap, g)
finalizeRandomGrowth originalSeed territory mapG
    | originalSeed /= originCoordinate = mapG
    | otherwise = closeLagoon mapG territory
    where
        closeLagoon currentMapG currentTerritory
            | null failingHexes = currentMapG
            | null additions = currentMapG
            | otherwise = closeLagoon nextMapG nextTerritory
            where
                board = fst currentMapG
                failingHexes = filter needsMoreSpace $ toList currentTerritory
                needsMoreSpace coord = occupiedNeighbors /= 6 && openNeighbors < 2
                    where
                        neighbors = adjacentCubeCoordinates coord
                        occupiedNeighbors = length $ filter (`member` board) neighbors
                        openNeighbors = length neighbors - occupiedNeighbors
                additions = closestToOriginSet $ fromList $ concatMap validNeighbors failingHexes
                validNeighbors coord = filter (`notMember` board) (adjacentCubeCoordinates coord)
                nextHex = Set.elemAt 0 additions
                nextTerritory = Set.insert nextHex currentTerritory
                nextMapG = (insert nextHex (TerrainHex False Lagoon []) board, snd currentMapG)

-- | Builds a full randomized terrain board and retries until the invariants hold.
createBoard :: RandomGen g => g -> HexBoard
createBoard g | failed = createBoard $ snd filledTerrainBoardG
              | otherwise = CubeCoordinateTokens (toHourHand (3 :: Int)) (setLargest connectedSets fullBoard)
    where   connectedSets = getConnectedSets fullBoard
            setLargest [] board = board
            setLargest ((_, []):rest) board = setLargest rest board
            setLargest ((_, large:_):rest) board = setLargest rest $
                foldr (alter toLargest) board (keys large)
            toLargest (Just (TerrainHex _ f ts)) = Just (TerrainHex True f ts)
            toLargest _ = Nothing
            fullBoard = fst $ addTokens 3 PalmTree hutBoardG
            hutBoardG = addTokens 4 Hut allStatueBoardG
            allStatueBoardG = addToken 4 (Statue $ toHourHand (1 :: Int)) twoStatueBoardG
            twoStatueBoardG = addToken 4 (Statue $ toHourHand (5 :: Int)) oneStatueBoardG
            oneStatueBoardG = addToken 4 (Statue $ toHourHand (9 :: Int)) filledTerrainBoardG
            (failed, filledTerrainBoardG) = fillInTerrain allTerrainBoardG
            allTerrainBoardG = fillHexOcean beachTerrainBoardG
            beachTerrainBoardG = growTerrainSeeds
                (CoastalGrowth, [6,5,4,3]) (TerrainHex False Beach []) meadowTerrainBoardG
            meadowTerrainBoardG = growTerrainSeeds
                (RandomGrowth, [10,6,4]) (TerrainHex False Meadow []) jungleTerrainBoardG
            jungleTerrainBoardG = growTerrainSeeds
                (RandomGrowth, [16,10,6]) (TerrainHex False Jungle []) mountainTerrainBoardG
            mountainTerrainBoardG = growTerrainSeeds
                (RandomGrowth, [12,8,6]) (TerrainHex False Mountain []) riverTerrainBoardG
            riverTerrainBoardG = growTerrainSeeds
                (RiverGrowth, [5,3,3]) (TerrainHex False River []) lagoonTerrainBoardG
            lagoonTerrainBoardG = growTerrainSeeds
                (RandomGrowth, [10,6,4]) (TerrainHex False Lagoon []) emptyHexMapG
            emptyHexMapG = (Map.empty :: HexMap, g)


-- | Constructs the uncensored initial game state, including decks, board, and opening hands.
createNewGameState :: [PlayerDescription] -> Int -> GameState
createNewGameState playerDs randomSeed
    | not validColors = initState & messageL %~ const "Player Colors Invalid"
    | length playerDs < 2 = initState & messageL %~ const "Game must be played with at least 2 players"
    | otherwise = nextTurn . dealClues $ initState
    where   initState = GameState
                { players = newPlayers
                , turn = 0
                , playerTurn = firstPlayer
                , activePlayer = firstPlayer
                , clueDeck = (randomizedClues, [])
                , treasureDeck = (stackedTreasure, [])
                , terrainBoard = board
                , treasureBoards = newTreasureBoard <$> take 4 allClueColors
                , raisingTreasure = Nothing
                , latestMessage = "Player " <> show firstPlayerName <> " Turn"
                , gameOver = False
                , seed = (randomSeed, 1)
                }
            validColors = all ((`elem` allPlayerColors) . (\pd -> pd.playerColor)) playerDs
            newPlayers = map createNewPlayer playerDs
            firstPlayer = maybe (error "createNewGameState requires at least one player") (\(p, _) -> p.player.playerId) $ uncons newPlayers
            firstPlayerName = maybe "Missing" (\(p, _) -> p.player.playerName) $ uncons newPlayers
            g = fst $ mkStdGenN randomSeed 0
            (randomizedClues, gT) = uniformShuffleList allClues g
            (stackedTreasure, gB) = stackTreasure gT
            board = createBoard gB
            startCards = if length playerDs == 2 then 6::Int else 4
            dealClues s = dealStartClues (map (.player) s.players) s
            dealStartClues [] s = s
            dealStartClues (pd:restPlayers) s = dealStartClues restPlayers $
                dealCluesToPlayer startCards pd.playerId s
            dealCluesToPlayer 0 _ s = s
            dealCluesToPlayer n pId s = dealCluesToPlayer (n-1) pId $
                s & messageL %~ eUpd & clueDeckL %~ deckUpd & playerL pId %~ psUpd
                where (eUpd, deckUpd, psUpd, _) = _eitherUpdates3 $
                        dealClueCardToPlayer (0, 0) s.clueDeck (findPlayer pId s)

-- | Converts an 'Either' update into a message updater and a value updater.
_eitherUpdates :: Semigroup a1 => Either a1 (a2 -> a2) -> (a1 -> a1, a2 -> a2)
_eitherUpdates (Left e) = ((e <>), id)
_eitherUpdates (Right upd) = (id, upd)

-- | Variant of '_eitherUpdates' for two coordinated update functions.
_eitherUpdates2 :: Semigroup a1 => Either a1 (a2 -> a2, a3 -> a3)
    -> (a1 -> a1, a2 -> a2, a3 -> a3)
_eitherUpdates2 (Left e) = ((e <>), id, id)
_eitherUpdates2 (Right (updA, updB)) = (id, updA, updB)

-- | Variant of '_eitherUpdates' for three coordinated update functions.
_eitherUpdates3 :: Semigroup a1 => Either a1 (a2 -> a2, a3 -> a3, a4 -> a4)
    -> (a1 -> a1, a2 -> a2, a3 -> a3, a4 -> a4)
_eitherUpdates3 (Left e) = ((e <>), id, id, id)
_eitherUpdates3 (Right (updA, updB, updC)) = (id, updA, updB, updC)

-- | Variant of '_eitherUpdates' for four coordinated update functions.
_eitherUpdates4 :: Semigroup a1 => Either a1 (a2 -> a2, a3 -> a3, a4 -> a4, a5 -> a5)
    -> (a1 -> a1, a2 -> a2, a3 -> a3, a4 -> a4, a5 -> a5)
_eitherUpdates4 (Left e) = ((e <>), id, id, id, id)
_eitherUpdates4 (Right (updA, updB, updC, updD)) = (id, updA, updB, updC, updD)

-- | Creates an empty treasure board for a single clue color.
newTreasureBoard :: ClueColor -> TreasureBoard
newTreasureBoard c = (c, [])

-- | Advances the game to the next player's turn and refreshes their action budget.
nextTurn :: GameState -> GameState
nextTurn gameState = assignPlayer mCurrentPlayer . setNotActive $ gameState
    where   playerId | gameState.turn == 0 = gameState.playerTurn
                     | otherwise = nextPlayer gameState.playerTurn gameState
            setNotActive s = s & playersL %~ map setNotPlayerTurn
            mCurrentPlayer = find ((playerId ==) . (.player.playerId)) gameState.players
            assignPlayer Nothing s = s & messageL %~ (("Could not find playerId " <> show playerId) <>)
            assignPlayer _ s =
                s & playerL playerId %~ setPlayerTurn
                  & playerTurnL %~ const playerId
                  & playerActiveL %~ const playerId
                  & turnL %~ (+1)

-- | Finds the next player in seating order, wrapping around to the first player.
nextPlayer :: PlayerId -> GameState -> PlayerId
nextPlayer currentPlayer gameState =
    case dropWhile (/= currentPlayer) playerIds of
        _ : nextId : _ -> nextId
        _ -> fromMaybe currentPlayer (viaNonEmpty head playerIds)
    where
        playerIds = map (.player.playerId) gameState.players

-- | Clears all remaining per-turn actions for a player.
setNotPlayerTurn :: PlayerState -> PlayerState
setNotPlayerTurn ps = ps    { availableJeepMoves = 0
                            , availableCluePlays = 0
                            , availableRemoveMarkers = 0
                            , availablePickupAmulet = 0
                            , availableClueCardExchange = 0 }

-- | Resets a player to the standard action budget for the start of a turn.
setPlayerTurn :: PlayerState -> PlayerState
setPlayerTurn ps = ps   { availableJeepMoves = 3
                        , availableCluePlays = 1
                        , availableRemoveMarkers = 0
                        , availablePickupAmulet = 1
                        , availableClueCardExchange = 1 }

-- | Derives a deterministic split generator at the requested depth.
mkStdGenN :: Int -> Int -> (StdGen, StdGen)
mkStdGenN seed 0 = splitGen $ mkStdGen seed
mkStdGenN seed n = splitGen (snd $ mkStdGenN seed (n-1))

-- | Draws one clue card for a player, reshuffling the discard pile when needed.
dealClueCardToPlayer :: (Int, Int) -> Deck ClueCard -> Either Text PlayerState
    -> Either Text (Deck ClueCard -> Deck ClueCard, PlayerState -> PlayerState, (Int, Int) -> (Int, Int))
dealClueCardToPlayer _ _ (Left e) = Left e
dealClueCardToPlayer gSeed (draw, discard) (Right pS)
    = withSeedUpd <$> bimapF toDeckUpd (cluesL %~) (moveCard Nothing id draw' pS.clues)
    where   withSeedUpd (deckUpd, pUpd) = (deckUpd, pUpd, seedUpd)
            toDeckUpd = if null draw then const . const (drop 1 randomizedClues, []) else first
            seedUpd = if null draw then second (+1) else id
            rG = fst $ mkStdGenN (snd gSeed) $ fst gSeed
            randomizedClues = fst $ uniformShuffleList discard rG
            draw' = if null draw then randomizedClues else draw

-- | Moves several cards by repeatedly applying 'moveCard'.
moveCards :: (Eq a, Show a, Eq b, Show b) => Int -> Maybe a -> (a -> b) -> [a] -> [b]
    -> Either Text ([a] -> [a], [b] -> [b])
moveCards 0 _ _ _ _ = Right (id, id)
moveCards n mSpecific convert from to = do
    (fromUpd, toUpd) <- moveCard mSpecific convert from to
    let nextFrom = fromUpd from
        nextTo = toUpd to
    (fromRest, toRest) <- moveCards (n-1) mSpecific convert nextFrom nextTo
    pure (fromRest . fromUpd, toRest . toUpd)

-- | Moves one card from a source list into a destination list.
moveCard :: (Eq a, Show a, Eq b, Show b) => Maybe a -> (a -> b) -> [a] -> [b]
    -> Either Text ([a] -> [a], [b] -> [b])
moveCard Nothing _  [] _ = Left "Cannot pop card from list"
moveCard Nothing toB (top:_) _ = Right (drop 1, (toB top:))
moveCard (Just c) toB fromL _ =
    case break (== c) fromL of
        (_, []) -> Left $ "Could not find card " <> show c
        (before, _:after) -> Right (const $ before <> after, (toB c :))

-- | Traversal targeting the player with the requested id.
playerL :: PlayerId -> Traversal' GameState PlayerState
playerL wantedId handler gameState =
    (\players -> gameState { players = players }) <$> traverse visit gameState.players
  where
    visit playerState
        | playerState.player.playerId == wantedId = handler playerState
        | otherwise = pure playerState

-- | Lens into the clue deck.
clueDeckL :: Lens' GameState ([ClueCard], [ClueCard])
clueDeckL = lens (.clueDeck) (\gameState clueDeck -> gameState { clueDeck = clueDeck })

-- | Lens into the treasure deck.
treasureDeckL :: Lens' GameState ([TreasureCard], [TreasureCard])
treasureDeckL = lens (.treasureDeck) (\gameState treasureDeck -> gameState { treasureDeck = treasureDeck })

-- | Lens into the latest game message.
messageL :: Lens' GameState Text
messageL = lens (.latestMessage) (\gameState message -> gameState { latestMessage = message })

-- | Lens into the player whose turn owns the turn counter.
playerTurnL :: Lens' GameState PlayerId
playerTurnL = lens (.playerTurn) (\gameState playerId -> gameState { playerTurn = playerId })

-- | Lens into the player currently allowed to act.
playerActiveL :: Lens' GameState PlayerId
playerActiveL = lens (.activePlayer) (\gameState playerId -> gameState { activePlayer = playerId })

-- | Lens into the full player list.
playersL :: Lens' GameState [PlayerState]
playersL = lens (.players) (\gameState players -> gameState { players = players })

-- | Lens into the turn counter.
turnL :: Lens' GameState Int
turnL = lens (.turn) (\gameState t -> gameState { turn = t })

-- | Lens into a player's clue hand.
cluesL :: Lens' PlayerState [ClueCard]
cluesL = lens (.clues) (\playerState clues -> playerState { clues = clues })

-- | Lens into the mutable terrain token map.
boardL :: Lens' GameState HexMap
boardL = lens (\gameState -> case gameState.terrainBoard of
                    CubeCoordinateTokens _ tokens -> tokens)
    updateBoard

gameOverL :: Lens' GameState Bool
gameOverL = lens (.gameOver) (\gameState over -> gameState { gameOver = over})

updateBoard :: GameState -> HexMap -> GameState
updateBoard gS board = case gS.terrainBoard of
    CubeCoordinateTokens o _ -> gS { terrainBoard = CubeCoordinateTokens o board}

-- | Lens into the pair of deterministic RNG counters.
seedL :: Lens' GameState (Int, Int)
seedL = lens (.seed)
    (\gameState seed -> gameState { seed = seed})

-- | Traversal targeting the treasure board for a single clue color.
treasureL :: ClueColor -> Traversal' GameState ClueBoard
treasureL wantedColor handler gameState =
    (\treasures -> gameState { treasureBoards = treasures }) <$> traverse visit gameState.treasureBoards
  where
    visit treasure@(color, board)
        | color == wantedColor = (color, ) <$> handler board
        | otherwise = pure treasure

-- | Lens into the currently active player id.
activePlayerL :: Lens' GameState PlayerId
activePlayerL = lens (.activePlayer)
    (\gameState activePlayer -> gameState { activePlayer = activePlayer})

-- | Lens into the optional treasure-raising state.
raiseTreasureL :: Lens' GameState (Maybe RaisingTreasureState)
raiseTreasureL = lens (.raisingTreasure)
    (\gameState raisingTreasure -> gameState { raisingTreasure = raisingTreasure})

-- | Looks up a player by id.
findPlayer :: PlayerId -> GameState -> Either Text PlayerState
findPlayer wantedId gameState = maybeToRight ("Could not find player " <> show wantedId) $
    find ((wantedId == ) . (.player.playerId)) gameState.players

-- | Looks up the treasure board for a clue color.
findTreasure :: ClueColor -> GameState -> Either Text ClueBoard
findTreasure color gameState = maybeToRight ("Could not find treasure " <> show color) $
    snd <$> find ((color ==) . fst) gameState.treasureBoards

-- | Adds the pass action when the active mode is the normal turn mode.
passTurnOption :: (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
passTurnOption (GameModeNominal, moves) = (GameModeNominal, PassTurn:moves)
passTurnOption modeMoves = modeMoves

-- | Switches the option enumeration mode when a treasure-raising sequence is active.
isRaisingTreasure :: GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
isRaisingTreasure gS (mode, moves) = case gS.raisingTreasure of
    Nothing -> (mode, moves)
    Just treasureState -> if not (null treasureState.rtViewing)
        then (GameModeRaisingTreasureView treasureState, moves)
        else (GameModeRaisingTreasureChoice treasureState, moves)

-- | Adds the available choice actions while resolving a raised treasure.
raisingTreasureChoiceCase :: PlayerState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
raisingTreasureChoiceCase player (GameModeRaisingTreasureChoice ts, moves)
    = (GameModeRaisingTreasureChoice ts, pushOptions moves)
    where   pushOptions = case (ts.rtTreasureChest, player.amulets > 0) of
                ((Curse:_, _), True) -> (RaisingTreasureAcceptCurse:) . (RaisingTreasureWardCurse:)
                ((Curse:_, _), False) -> (RaisingTreasureAcceptCurse:)
                _ -> (RaisingTreasurePass:) . (RaisingTreasureTake:)
raisingTreasureChoiceCase _ gMoves = gMoves

-- | Restricts a treasure viewer to passing after inspecting their temporary card.
raisingTreasureViewCase :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
raisingTreasureViewCase _ _ (GameModeRaisingTreasureView ts, moves) =
    (GameModeRaisingTreasureView ts, RaisingTreasurePass:moves)
raisingTreasureViewCase _ _ gMoves = gMoves

-- | Filters board locations by a predicate over index and hex.
findLocations :: (CubeCoordinate Int -> TerrainHex -> Bool) -> HexMap -> HexMap
findLocations = Map.filterWithKey

getHexMap :: HexBoard -> HexMap
getHexMap (CubeCoordinateTokens _ m) = m

-- | Like 'findLocations', but returns 'Nothing' when no matches are found.
mFindLocationList :: (CubeCoordinate Int -> TerrainHex -> Bool) -> HexMap
    -> Maybe (NonEmpty (CubeCoordinate Int, TerrainHex))
mFindLocationList filt board = nonEmpty . toPairs $ findLocations filt board

-- | Matches hexes that contain the requested token.
hasToken :: TerrainToken -> CubeCoordinate Int -> TerrainHex -> Bool
hasToken t _ (TerrainHex _ _ ts) = t `elem` ts

-- | Returns the first location containing the requested token, if any.
findFirstToken :: TerrainToken -> HexMap -> Maybe (CubeCoordinate Int, TerrainHex)
findFirstToken t = (head <$>) . mFindLocationList (hasToken t)

-- | Adds valid 'RaiseTreasure' actions for clue colors uniquely identified by current markers.
raiseTreasureCase :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
raiseTreasureCase player gS (GameModeNominal, moves) =
    (GameModeNominal, (RaiseTreasure <$> validColors) ++ moves)
    where   validColors = mapMaybe mColor $ filter exactlyOne possibleColorTokens
            jeepHex = findFirstToken (PlayerJeep player.player.playerId) (getHexMap gS.terrainBoard)
            tokenList = maybe [] (\(_, TerrainHex _ _ ts) -> ts) jeepHex
            possibleColorTokens = filter (isJust . mColor) tokenList
            mColor (ClueToken t) = Just t
            mColor _ = Nothing
            exactlyOne t = ((1 ==) . length) $ findLocations (hasToken t) (getHexMap gS.terrainBoard)
raiseTreasureCase _ _ gMoves = gMoves

-- | Adds the amulet pickup action when the jeep is on an amulet and pickup is still available.
pickupAmuletCase :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
pickupAmuletCase player gS (GameModeNominal, moves) = (GameModeNominal, possibleAmulet moves)
    where   jeepHex = findFirstToken (PlayerJeep player.player.playerId) (getHexMap gS.terrainBoard)
            tokenList = maybe [] (\(_, TerrainHex _ _ ts) -> ts) jeepHex
            possibleAmulet | player.availablePickupAmulet > 0 && Amulet `elem` tokenList = (PickupAmulet:)
                           | otherwise = id
pickupAmuletCase _ _ gMoves = gMoves

-- | Adds amulet-powered substitute actions when the player still has amulets.
useAmuletCase :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
useAmuletCase player gS (GameModeNominal, moves) | player.amulets == 0 = (GameModeNominal, moves)
                                                 | otherwise = foldr ($) (GameModeNominal, moves) enumerators
    where   enumerators =   [ addAmuletMoveOption
                            , addAmuletExchangeCards
                            , addAmuletPlayClue
                            , addAmuletRemoveSiteMarker ]
            addAmuletMoveOption = if player.availableJeepMoves == 0 then second (UseAmuletIncrMove:) else id
            addAmuletExchangeCards = if player.availableClueCardExchange == 0 then second (UseAmuletExchangeCards:) else id
            addAmuletPlayClue = if player.availableCluePlays == 0 then second (possibleCluePlays ++) else id
            addAmuletRemoveSiteMarker = second (possibleRemoveMarkers ++)
            possibleCluePlays = map convertPlay
                (snd $ enumeratePossibleCluePlays (player { availableCluePlays = 1 }) gS (GameModeNominal, []))
            convertPlay (PlayClue color clue) = UseAmuletPlayClue color clue
            convertPlay p = p
            colorToLocations = uncurry findLocations . (, getHexMap gS.terrainBoard) . hasToken . ClueToken
            colorLocations = filter ((> 1) . length . snd) $ zip allClueColors (map colorToLocations allClueColors)
            possibleRemoveMarkers = concatMap (asRemovePlay . second keys) colorLocations
            asRemovePlay (c, locs) = map (uncurry (UseAmuletRemoveSiteMarker c) . toPair) locs
useAmuletCase _ _ gMoves = gMoves

-- | Interprets a clue object against a concrete board location.
matchObject :: ClueObject -> CubeCoordinate Int -> TerrainHex -> Bool
matchObject (FeatureClue True clueF) _ (TerrainHex isL tF _) = isL && clueF == tF
matchObject (FeatureClue _ clueF) _ (TerrainHex _ tF _) = clueF == tF
matchObject (TokenClue clueT) _ (TerrainHex _ _ ts) = clueT `elem` ts
matchObject StatueClue _ (TerrainHex _ _ ts) = StatueClue `elem` mapMaybe toStatueClue ts
    where   toStatueClue (Statue _) = Just StatueClue
            toStatueClue _ = Nothing

-- | Tests whether a board location satisfies a clue card in the context of the full board.
matchClueCard :: HexMap -> ClueCard -> CubeCoordinate Int -> TerrainHex -> Bool
matchClueCard _ HiddenClue _ _ = False
matchClueCard board (WithinStepsOf n obj) k v
    = matchClueCard board (IsNotOn obj) k v && (not . null) (findLocations isObj bounds)
    where   isObj = matchObject obj
            bounds = Map.restrictKeys board boundSet
            boundSet = distanceSet n (one k) `Set.difference` one k
matchClueCard board (NotWithinStepsOf n obj) k _ = null (findLocations isObj bounds)
    where   isObj = matchObject obj
            bounds = Map.restrictKeys board boundSet
            boundSet = distanceSet n (one k) `Set.difference` one k
matchClueCard _ (IsOn obj) k v = matchObject obj k v
matchClueCard _ (IsNotOn obj) k v = not (matchObject obj k v)

-- | Applies a clue card to a clue color, returning the candidate set before and after filtering.
applyClue :: HexMap -> (ClueColor, ClueCard) -> (HexMap, HexMap)
applyClue board (color, card) = (beforeMarkers, afterMarkers)
    where   currentMarkers = findLocations (hasToken $ ClueToken color) board
            beforeMarkers   | null currentMarkers = findLocations isNonOceanTerrain board
                            | otherwise = currentMarkers
            afterMarkers = findLocations (matchCard card) beforeMarkers
            matchCard = matchClueCard board

-- | Rewrites clue markers in place while preserving every board hex and non-clue token.
applyClueToBoard :: HexMap -> (ClueColor, ClueCard) -> HexMap
applyClueToBoard board (color, card) = foldr updateMarker board markerCoords
        where
                (beforeMarkers, afterMarkers) = applyClue board (color, card)
                markerCoords = keys $ Map.union beforeMarkers afterMarkers
                updateMarker coord = alter (updateHex <$>) coord
                    where
                        updateHex (TerrainHex isLargest feature tokens) =
                                TerrainHex isLargest feature updatedTokens
                            where
                                withoutColor = filter (/= ClueToken color) tokens
                                updatedTokens
                                        | coord `member` afterMarkers = ClueToken color : withoutColor
                                        | otherwise = withoutColor

-- | Adds legal clue-play actions that would strictly narrow the chosen clue board.
enumeratePossibleCluePlays :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
enumeratePossibleCluePlays player _ (GameModeNominal, moves)
    | player.availableCluePlays == 0 = (GameModeNominal, moves)
enumeratePossibleCluePlays player gS (GameModeNominal, moves) = (GameModeNominal, allOptions ++ moves)
    where   allOptions = concatMap (map toM . filter possible . zip (take 4 allClueColors) . repeat) player.clues
            toM (color, card) = PlayClue color card
            possible cCard = let (before, after) = applyClue (getHexMap gS.terrainBoard) cCard in
                length before > length after && not (null after)
enumeratePossibleCluePlays _ _ gMoves = gMoves

-- | Adds legal jeep moves, falling back to unconstrained placement if the jeep is missing.
enumeratePossibleJeepMoves :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
enumeratePossibleJeepMoves player gS (GameModeNominal, moves)
    | player.availableJeepMoves == 0 = (GameModeNominal, moves)
    | null oneLeg = (GameModeMustMoveJeep, map toMove unconstrained)
    | otherwise = (GameModeNominal, map toMove oneLeg)
    where   connectedSets = concatMap snd (filter ((Ocean /=) . fst) $ getConnectedSets (getHexMap gS.terrainBoard))
            jeepHex = findFirstToken (PlayerJeep player.player.playerId) (getHexMap gS.terrainBoard)
            oneLeg = case jeepHex of (Just (k, _)) -> toList $ Set.delete k (legSet k)
                                     Nothing -> []
            legSet k = Set.intersection boardCoords $ Set.unions $ adjacentCoords : territorySets
                where   adjacentCoords = distanceSet 1 (one k)
                        territorySets = map (fromList . keys) (filter (member k) connectedSets)
            boardCoords = fromList $ keys $ Map.filter isNonOceanHex (getHexMap gS.terrainBoard)
            isNonOceanHex (TerrainHex _ feature _) = feature /= Ocean
            unconstrained = map fst $ filter ((\(TerrainHex _ f _) -> f /= Ocean) . snd) $
                toPairs (getHexMap gS.terrainBoard)
            toMove coord = uncurry MoveJeep (toPair coord)
enumeratePossibleJeepMoves _ _ gMoves = gMoves


-- | Enumerates the active player's legal moves by layering the mode-specific option builders.
enumeratePlayerOptions :: PlayerState -> GameState -> (GameMode, [PlayerMove])
enumeratePlayerOptions player gS = foldr ($) (GameModeNominal, []) enumerators
    where   enumerators =   [ passTurnOption
                            , enumeratePossibleCluePlays player gS
                            , useAmuletCase player gS
                            , pickupAmuletCase player gS
                            , raiseTreasureCase player gS
                            , raisingTreasureChoiceCase player
                            , raisingTreasureViewCase player gS
                            , enumeratePossibleJeepMoves player gS
                            , isRaisingTreasure gS
                            ]

-- | Enumerates the legal moves for the currently active player.
enumerateActivePlayerOptions :: GameState -> [PlayerMove]
enumerateActivePlayerOptions gS = enumerateMoves ePlayer
    where   ePlayer = findPlayer gS.activePlayer gS
            enumerateMoves (Left e) = [PlayerMoveError e]
            enumerateMoves (Right player) = case enumeratePlayerOptions player gS of
                (GameModeError e, moves) -> PlayerMoveError e:moves
                (_, moves) -> moves


-- | Validates a clue play and returns the coordinated updates needed to apply it.
playClueAndUpdate :: ClueColor -> ClueCard -> PlayerId -> GameState
    -> Either Text (HexMap -> HexMap, PlayerState -> PlayerState, ClueBoard -> ClueBoard)
playClueAndUpdate color card pId gS = eVerifyCard <*> eUpdates
    where   ePlayer = findPlayer pId gS
            eClueBoard = findTreasure color gS
            eVerifyCard = id <$ (findCard =<< ePlayer)
            findCard player = if card `elem` player.clues then Right card
                else Left $ "Could not find " <> show card <> " in player " <> show pId <> " clues."
            boardUpd = flip applyClueToBoard (color, card)
            eClues = (.clues) <$> ePlayer
            eCluesAndBoard = (,) <$> eClues <*> eClueBoard
            eCardUpds = uncurry (moveCard (Just card) (pId, )) =<< eCluesAndBoard
            updPlayer playerCardUpd player = player { clues = playerCardUpd player.clues }
            eUpdates = (\(psU, bU) -> (boardUpd, updPlayer psU, bU)) <$> eCardUpds

-- | Deals several clue cards directly to a player, updating deck state and seed counters.
dealCardsDirect :: Int -> PlayerId -> GameState -> GameState
dealCardsDirect 0 _ gS = gS
dealCardsDirect n pId gS =
    currentState & messageL %~ eDeckUpd & clueDeckL %~ deckUpd & playerL pId %~ psUpd & seedL %~ seedUpd
    where   currentState = dealCardsDirect (n-1) pId gS
            (eDeckUpd, deckUpd, psUpd, seedUpd) = _eitherUpdates3 $
                dealClueCardToPlayer currentState.seed currentState.clueDeck (findPlayer pId currentState)

-- | Rewrites the token list at one board location when the index exists.
alterTokenList :: ([TerrainToken] -> [TerrainToken]) -> Maybe (CubeCoordinate Int) -> HexMap -> HexMap
alterTokenList updTs (Just k) = alter (updateTokenList <$>) k
    where updateTokenList (TerrainHex isL f ts) = TerrainHex isL f (updTs ts)
alterTokenList _ Nothing = id

-- | Applies a clue play immediately, including replacement draw on success.
playClueDirect :: ClueColor -> ClueCard -> GameState -> GameState
playClueDirect color card gS =
    if isRight ePlayClueUpds then dealCardsDirect 1 pId updatedState else updatedState
    where   pId = gS.activePlayer
            ePlayClueUpds = playClueAndUpdate color card pId gS
            (eUpd, markerUpd, playedCardUpd, treasureBoardUpd) = _eitherUpdates3 ePlayClueUpds
            updatedState =
                gS & messageL %~ eUpd & boardL %~ markerUpd & playerL pId %~ playedCardUpd
                   & treasureL color %~ treasureBoardUpd

-- | Moves the active player's jeep token to the requested board coordinates.
moveJeepDirect :: Int -> Int -> GameState -> GameState
moveJeepDirect i j gS =
    gS  & boardL %~ addJeep & boardL %~ removeJeep
    where   jeep = PlayerJeep gS.activePlayer
            mJeepHex = fst <$> findFirstToken jeep (getHexMap gS.terrainBoard)
            removeJeep = alterTokenList (filter (/= jeep)) mJeepHex
            addJeep = alterTokenList (jeep:) (Just $ mkCubeCoordinate i j)

-- | Discards the active player's clue hand and redraws four fresh clue cards.
exchangeCardsDirect :: GameState -> GameState
exchangeCardsDirect gS =
    if isRight eDiscardUpds then dealCardsDirect 4 pId updatedState else updatedState
    where   pId = gS.activePlayer
            (errDiscardUpd, toDiscardUpd, playerCluesUpd) = _eitherUpdates2 eDiscardUpds
            clueUpd = second toDiscardUpd
            playerDiscardUpd player = player { clues = playerCluesUpd player.clues }
            ePlayer = findPlayer pId gS
            eDiscardUpds = moveCards 4 Nothing id (either (const []) (.clues) ePlayer) (snd gS.clueDeck)
            updatedState =
                gS & messageL %~ errDiscardUpd & clueDeckL %~ clueUpd
                   & playerL pId %~ playerDiscardUpd

-- | Deals one treasure card face-up into a player's temporary viewing area.
dealTreasureCardDirect :: PlayerId -> GameState -> GameState
dealTreasureCardDirect anId s = s & playerL anId %~ updTCards & treasureDeckL %~ updTDeck
    where   updTCards player = player { viewingTreasures = topCard ++ player.viewingTreasures }
            topCard = take 1 $ fst s.treasureDeck
            updTDeck = first $ drop 1

-- | Shuffles the treasure chest after all viewers have returned their cards.
raiseTreasureShuffle :: GameState -> GameState
raiseTreasureShuffle s =
    s & raiseTreasureL %~ (shuffleTCards <$>) & seedL %~ updSeed & raiseTreasureChooseStart
    where   cards = maybe [] (fst . (.rtTreasureChest)) s.raisingTreasure
            updSeed = second (+1)
            rG = fst $ mkStdGenN (snd s.seed) $ fst s.seed
            randomizedTreasures = fst $ uniformShuffleList cards rG
            shuffleTCards rt = rt { rtTreasureChest = (randomizedTreasures, [])}

-- | Ends the treasure-raising sequence, recalculates scores, and marks winners if the chest is empty.
raiseTreasureFinished :: GameState -> GameState
raiseTreasureFinished s =
    s & clearRaising & setActivePlayer & playersL %~ (scorePlayer <$>) & checkGameWinner
    where   mCards :: Maybe (NonEmpty TreasureCard)
            mCards = do
                raising <- s.raisingTreasure
                nonEmpty (fst raising.rtTreasureChest)
            clearRaising gS = gS { raisingTreasure = Nothing }
            toInt (Treasure i) = i
            toInt _ = 0
            score player = player.amulets + 10 * sum (map toInt player.foundTreasures)
            scorePlayer player = player { score = CurrentScore $ score player}
            maxS = maybe 0 maximum1 $ nonEmpty (score <$> s.players)
            checkGameWinner gS  | isNothing mCards = gS & playersL %~ (setWinner <$>) & gameOverL %~ const True
                                | otherwise = gS
            setWinner player = player { score = if CurrentScore maxS == player.score
                                                then WinnerScore maxS else player.score }

-- | Advances treasure choice to the next eligible chooser, or finishes if none remain.
raiseTreasureNextChooser :: GameState -> GameState
raiseTreasureNextChooser s  | isNothing mNextPlayerIndex = s & raiseTreasureFinished
                            | otherwise = s & raiseTreasureL %~ (updChooser <$>) & setActivePlayer
    where   mNextPlayerIndex = do
                raising <- s.raisingTreasure
                _ <- nonEmpty (fst raising.rtTreasureChest)
                neOrder <- nonEmpty raising.rtOrder
                let order = toList neOrder
                    rtPlayerIndex = raising.rtPlayerIndex
                    currentChooser = fromMaybe (head neOrder) $ order !!? (rtPlayerIndex `mod` length order)
                nextDistinctIndex currentChooser (rtPlayerIndex + 1) order

            updChooser rt = rt { rtPlayerIndex = fromMaybe 0 mNextPlayerIndex }
            nextDistinctIndex _ _ [] = Nothing
            nextDistinctIndex currentChooser startIndex order =
                go startIndex (drop startIndex order)
              where
                go _ [] = Nothing
                go index (playerId:rest)
                    | playerId /= currentChooser = Just index
                    | otherwise = go (index + 1) rest

-- | Resets treasure choice to the start of the chooser order, or finishes if no choices remain.
raiseTreasureChooseStart :: GameState -> GameState
raiseTreasureChooseStart s  | isNothing mOrder = s & raiseTreasureFinished
                            | otherwise = s & raiseTreasureL %~ (updChooser <$>) & setActivePlayer
    where   mOrder = do
                raising <- s.raisingTreasure
                _ <- nonEmpty (fst raising.rtTreasureChest)
                nonEmpty raising.rtOrder
            updChooser rt = rt { rtPlayerIndex = 0 }

-- | Assigns the acting player based on the current turn or treasure-raising subphase.
setActivePlayer :: GameState -> GameState
setActivePlayer gS
    | isNothing gS.raisingTreasure = gS & activePlayerL %~ const gS.playerTurn
    | isJust mViewer || isJust mChooser = gS & activePlayerL %~ maybe id const (mViewer <|> mChooser)
    | otherwise = gS & messageL %~ ("Error no viewers or chooers in Treasure State" <>)
    where   mViewer = do
                raising <- gS.raisingTreasure
                neViewing <- nonEmpty raising.rtViewing
                pure (head neViewing)
            mChooser = do
                raising <- gS.raisingTreasure
                neOrder <- nonEmpty raising.rtOrder
                let rtPlayerIndex = raising.rtPlayerIndex `mod` length neOrder
                neAtPlayerIndex <- nonEmpty $ drop rtPlayerIndex (toList neOrder)
                pure (head neAtPlayerIndex)

-- | Returns any viewed treasure cards to the chest and advances the viewer queue.
raiseTreasureViewPass :: GameState -> GameState
raiseTreasureViewPass s =
    s   & playerL pId %~ updTCards & raiseTreasureL %~ (updTreasureState <$>)
        & setActivePlayer & if lastViewer then raiseTreasureShuffle else id
    where   pId = s.activePlayer
            ePlayer = findPlayer pId s
            playerCards = either (const []) (.viewingTreasures) ePlayer
            updTCards player = player { viewingTreasures = [] }
            lastViewer = maybe False ((== 1) . length . (.rtViewing)) mRaising
            mRaising = s.raisingTreasure
            updTreasureState rt =
                rt
                    { rtTreasureChest = first (playerCards ++) rt.rtTreasureChest
                    , rtViewing = filter (/= pId) rt.rtViewing
                    }

-- | Applies a move only if it is currently legal; otherwise records an error message.
makeMoveParanoid :: GameState -> PlayerMove -> (GameState, [CensoredGameState])
makeMoveParanoid gS move | move `elem` enumerateActivePlayerOptions gS = makeMoveDirect gS move
                         | otherwise = censorGame $ gS & messageL %~ appendError
    where e = "Move " <> makeMoveSummary gS move <> " is not allowed for current player."
          appendError "" = e
          appendError message = message <> " | " <> e

makeMoveSummary :: GameState -> PlayerMove -> Text
makeMoveSummary _ (PlayerMoveError e) = "PlayerMoveError " <> e
makeMoveSummary _ PassTurn = "PassTurn"
makeMoveSummary _ (PlayClue color card) = "PlayClue " <> show color <> " " <> show card
makeMoveSummary gS (MoveJeep i j) = "MoveJeep " <> show (i, j) <> " " <> formatMoveTokens (tokensAtDestination gS i j)
makeMoveSummary _ ExchangeClueCards = "ExchangeClueCards"
makeMoveSummary _ PickupAmulet = "PickupAmulet"
makeMoveSummary _ UseAmuletIncrMove = "UseAmuletIncrMove"
makeMoveSummary _ (UseAmuletPlayClue color card) = "UseAmuletPlayClue " <> show color <> " " <> show card
makeMoveSummary _ UseAmuletExchangeCards = "UseAmuletExchangeCards"
makeMoveSummary _ (UseAmuletRemoveSiteMarker color i j) = "UseAmuletRemoveSiteMarker " <> show color <> " " <> show (i, j)
makeMoveSummary _ (RaiseTreasure color) = "RaiseTreasure " <> show color
makeMoveSummary _ RaisingTreasurePass = "RaisingTreasurePass"
makeMoveSummary _ RaisingTreasureTake = "RaisingTreasureTake"
makeMoveSummary _ RaisingTreasureWardCurse = "RaisingTreasureWardCurse"
makeMoveSummary _ RaisingTreasureAcceptCurse = "RaisingTreasureAcceptCurse"

tokensAtDestination :: GameState -> Int -> Int -> [TerrainToken]
tokensAtDestination gS i j = maybe [] tokensAtCoord $ Map.lookup (mkCubeCoordinate i j) (getHexMap gS.terrainBoard)
    where tokensAtCoord (TerrainHex _ _ ts) = ts

formatMoveTokens :: [TerrainToken] -> Text
formatMoveTokens ts = "tokens=" <> show ts

makeMove :: GameState -> PlayerMove -> (GameState, [CensoredGameState])
makeMove gS move = makeMoveParanoid (gS & messageL %~ const (makeMoveSummary gS move)) move

-- | Returns 'True' when treasure resolution is waiting on viewers rather than choosers.
isViewingTreasure :: GameState -> Bool
isViewingTreasure gS = mMode == Just True
    where   mMode = do
                raising <- gS.raisingTreasure
                let isViewing = not . null $ raising.rtViewing
                pure isViewing

-- | Returns 'True' when the current chooser is the last remaining chooser in order.
isLastChooser :: GameState -> Bool
isLastChooser gS = mMode == Just True
    where   mMode = do
                raising <- gS.raisingTreasure
                neOrder <- nonEmpty raising.rtOrder
                let order = toList neOrder
                    rtPlayerIndex = raising.rtPlayerIndex `mod` length order
                    currentChooser = fromMaybe (head neOrder) $ order !!? rtPlayerIndex
                    remainingChoosers = drop (rtPlayerIndex + 1) order
                    isLastChoice = all (== currentChooser) remainingChoosers
                pure isLastChoice

-- | Executes a move without checking legality and returns fresh censored views.
makeMoveDirect :: GameState -> PlayerMove -> (GameState, [CensoredGameState])
makeMoveDirect gS (PlayerMoveError e) = censorGame $ gS & messageL %~ (e <>)
makeMoveDirect gS PassTurn = censorGame $ nextTurn gS
makeMoveDirect gS (PlayClue color card) = censorGame $
    gS & playClueDirect color card & playerL pId %~ playerUsedOptions
    where   pId = gS.activePlayer
            playerUsedOptions player = player { availableJeepMoves = 0
                                              , availableCluePlays = 0
                                              , availableRemoveMarkers = 0
                                              , availableClueCardExchange = 0}
makeMoveDirect gS (MoveJeep i j) = censorGame $
    gS & moveJeepDirect i j & playerL pId %~ playerUsedOptions
    where   pId = gS.activePlayer
            isForcedPlacement = isNothing $ findFirstToken (PlayerJeep pId) (getHexMap gS.terrainBoard)
            playerUsedOptions
                | isForcedPlacement = id
                | otherwise = \player -> player { availableJeepMoves = player.availableJeepMoves - 1
                                                , availableCluePlays = 0
                                                , availableRemoveMarkers = 0
                                                , availableClueCardExchange = 0}
makeMoveDirect gS ExchangeClueCards = censorGame $
    gS & exchangeCardsDirect & playerL pId %~ playerUsedOptions
    where   pId = gS.activePlayer
            playerUsedOptions player = player { availableJeepMoves = 0,
                                                availableCluePlays = 0,
                                                availableRemoveMarkers = 0,
                                                availableClueCardExchange = 0}
makeMoveDirect gS PickupAmulet = censorGame $
    gS  & messageL %~ errorUpd & boardL %~ boardUpd & playerL pId %~ playerUsedOptions
    where   pId = gS.activePlayer
            ePlayer = findPlayer pId gS
            jeep = PlayerJeep gS.activePlayer
            mJeepHex = findFirstToken jeep (getHexMap gS.terrainBoard)
            amuletUpd = alterTokenList (filter (/= Amulet)) (fst <$> mJeepHex)
            incrAmulet player = player { amulets = player.amulets + 1,
                                         availablePickupAmulet = 0}
            errorUpd    | isLeft ePlayer = (fromLeft "" ePlayer <>) :: (Text -> Text)
                        | isNothing mJeepHex = (("Could not find " <> show pId <> " jeep.") <> )
                        | otherwise = id
            hasError = isLeft ePlayer || isNothing mJeepHex
            boardUpd = if hasError then id else amuletUpd
            playerUsedOptions = if hasError then id else incrAmulet
makeMoveDirect gS UseAmuletIncrMove = censorGame $
    gS & messageL %~ errorUpd & playerL pId %~ playerUpd
    where   pId = gS.activePlayer
            ePlayer = do
                player <- findPlayer pId gS
                if player.amulets == 0
                    then Left "Player has no remaining amulets"
                    else Right playerUsedOptions
            playerUsedOptions player = player { availableJeepMoves = player.availableJeepMoves + 3
                                              , amulets = player.amulets - 1}
            (errorUpd, playerUpd) = _eitherUpdates ePlayer
makeMoveDirect gS (UseAmuletPlayClue color card) = censorGame $
    gS & playClueDirect color card & playerL pId %~ playerUsedOptions
    where   pId = gS.activePlayer
            playerUsedOptions player = player { amulets = player.amulets - 1 }
makeMoveDirect gS UseAmuletExchangeCards = censorGame $
    gS & exchangeCardsDirect & playerL pId %~ playerUsedOptions
    where   pId = gS.activePlayer
            playerUsedOptions player = player { amulets = player.amulets - 1 }
makeMoveDirect gS (UseAmuletRemoveSiteMarker color i j) = censorGame $
    gS & boardL %~ boardUpd & playerL pId %~ playerUsedOptions
    where   pId = gS.activePlayer
            playerUsedOptions player = player { amulets = player.amulets - 1 }
            boardUpd = alterTokenList (filter (/= ClueToken color)) (Just $ mkCubeCoordinate i j)
makeMoveDirect gS (RaiseTreasure color) = censorGame $
    gS  & treasureDeckL %~ first (drop 1) & initTreasure
        & dealTreasureCards pTurns & treasureL color %~ const [] & boardL %~ (clearColor <$>)
        & nextTurn & setActivePlayer
    where   pId = gS.activePlayer
            pTurns = case filter ((color ==) . fst) gS.treasureBoards of
                [(_, playerIds)] -> pId:map fst playerIds
                _ -> [pId]
            viewTurns = uniquePlayers pTurns
            topCard = take 1 $ fst gS.treasureDeck
            initTreasure s = s { raisingTreasure = Just initialRaisingTreasureState }
            initialRaisingTreasureState = RaisingTreasureState
                { rtTreasureChest = (topCard, [])
                , rtOrder = pTurns
                , rtPlayerIndex = 0
                , rtViewing = viewTurns}
            clearColor (TerrainHex isL f ts) = TerrainHex isL f (filter (/= ClueToken color) ts)
            uniquePlayers = go []
            go _ [] = []
            go seen (playerId:rest)
                | playerId `elem` seen = go seen rest
                | otherwise = playerId : go (playerId : seen) rest
            dealTreasureCards [] s = s
            dealTreasureCards (anId:rest) s =
                s & dealTreasureCards rest & dealTreasureCardDirect anId
makeMoveDirect gS RaisingTreasurePass
    | isViewingTreasure gS = censorGame $ gS & raiseTreasureViewPass
    | isLastChooser gS = censorGame $ gS & raiseTreasureL %~ (discardTop <$>) & raiseTreasureChooseStart
    | otherwise = censorGame $ gS & raiseTreasureNextChooser
    where   discardTop rT = rT { rtTreasureChest = discardOne rT.rtTreasureChest }
            discardOne (p, d) = (drop 1 p, take 1 p ++ d)
makeMoveDirect gS RaisingTreasureTake = censorGame $
    gS  & raiseTreasureL %~ (removeTopT <$>) & raiseTreasureL %~ (removeTaker <$>)
        & playerL takerId %~ giveTopT & raiseTreasureChooseStart
    where   mRaising = gS.raisingTreasure
            giveTopT player = player { foundTreasures = appTop player.foundTreasures }
            appTop = maybe id ((<>) . take 1 . fst . (.rtTreasureChest)) mRaising
            playerIndex = maybe 0 (.rtPlayerIndex) mRaising
            takerId = gS.activePlayer
            removeTopT rT = rT { rtTreasureChest = first (drop 1) rT.rtTreasureChest }
            removeTaker rT = rT { rtPlayerIndex = 0
                                , rtOrder = removePlayerIndex rT.rtOrder }
            removePlayerIndex pList = take playerIndex pList <> drop (playerIndex + 1) pList
makeMoveDirect gS RaisingTreasureWardCurse
    | isLastChooser gS = censorGame $ gS & playerL pId %~ decrAmulet & raiseTreasureFinished
    | otherwise = censorGame $ gS & playerL pId %~ decrAmulet & raiseTreasureNextChooser
    where   pId = gS.activePlayer
            decrAmulet player = player { amulets = player.amulets - 1 }
makeMoveDirect gS RaisingTreasureAcceptCurse
    | isLastChooser gS = censorGame $ gS & playerL pId %~ loseTreasure & raiseTreasureFinished
    | otherwise = censorGame $ gS & playerL pId %~ loseTreasure & raiseTreasureNextChooser
    where   pId = gS.activePlayer
            loseTreasure player = player { foundTreasures = loseMax $ nonEmpty player.foundTreasures }
            loseMax :: Maybe (NonEmpty TreasureCard) -> [TreasureCard]
            loseMax Nothing = []
            loseMax (Just ts) = let maxT = maximum1 ts
                                    lTs = toList ts in
                drop 1 (filter (== maxT) lTs) <> filter (/= maxT) lTs

heuristicHint :: Int -> GameState -> [PlayerMove] -> [(Int, PlayerMove)]
heuristicHint level gS = map heuristicLevel
    where   heuristicLevel move = (heuristic level move, move)
            heuristic _ (PlayerMoveError _) = 0
            heuristic _ PassTurn = -10
            heuristic _ (PlayClue color card) = if null singleMarkers then reduction gS.terrainBoard color card else 0
            heuristic _ ExchangeClueCards = if null singleMarkers then 8 else -4
            heuristic _ PickupAmulet = 11
            heuristic _ UseAmuletIncrMove = -11
            heuristic _ (UseAmuletPlayClue _ _) = -11
            heuristic _ UseAmuletExchangeCards = -11
            heuristic _ (UseAmuletRemoveSiteMarker color _ _) = if null singleMarkers && color `elem` binaryColors then 6 else -4
            heuristic _ (RaiseTreasure _) = 1000
            heuristic _ (MoveJeep i j)
                | isTargetDestination i j = 100
                | null singleMarkers = amuletMoveScore i j
                | otherwise = 40 - minDistTo isSingleMarkerToken gS.terrainBoard i j
            heuristic _ RaisingTreasurePass = -10
            heuristic _ RaisingTreasureTake = 10
            heuristic _ RaisingTreasureWardCurse = 5
            heuristic _ RaisingTreasureAcceptCurse = 0
            amuletMoveScore i j =
                if amulets == 0 then -4
                    else 40 - minDistTo (Amulet ==) gS.terrainBoard i j
            amulets = countTokens gS (Amulet ==)
            isTargetDestination i j = any isHeuristicTarget (getTokens i j gS.terrainBoard)
            isHeuristicTarget Amulet = True
            isHeuristicTarget token = isSingleMarkerToken token
            getTokens i j (CubeCoordinateTokens _ m) = concatMap tokensOf $ findLocations (findAt i j) m
            isSingleMarkerToken (ClueToken c) = c `elem` singleMarkers
            isSingleMarkerToken _ = False
            findAt i j c _ = mkCubeCoordinate i j == c
            tokensOf (TerrainHex _ _ ts) = ts
            -- minDistTo computes the minimum distance between the hex at (i, j)
            -- and the nearest hex with one of the colors in colors on the HexMap board
            minDistTo tFilt board i j
                | any tFilt destinationTokens = -200
                | otherwise = fromMaybe 40 nearestDistance
                where
                    destinationTokens = getTokens i j board
                    targetCoords =
                        map fst
                            . filter (any tFilt . tokensOf . snd)
                            $ toPairs targetBoard
                    targetBoard = findLocations hasTargetToken hexMap
                    hasTargetToken _ (TerrainHex _ _ ts) = any tFilt ts
                    nearestDistance =
                        viaNonEmpty (floor . negate . maximum1)
                            ([ negate (cubeCoordinateDistance (mkCubeCoordinate i j) coord)
                             | coord <- targetCoords ])
                    hexMap = getHexMap board
            colorCounts = zip allClueColors $ map (countTokens gS . (==) . ClueToken) allClueColors
            singleMarkers = map fst $ filter ((== 1) . snd) colorCounts
            binaryColors = map fst $ filter ((== 2) . snd) colorCounts
            -- reduction uses the applyClue function to compare the size of before and after 
            -- and return the difference in size between before and after
            reduction board color card = length before - length after
                where
                    (before, after) = applyClue (getHexMap board) (color, card)

summary :: GameState -> Text
summary gameState = Text.intercalate " | " summaryParts
    where
        summaryParts = catMaybes
            [ latestMessagePart
            , Just $ "turn=" <> show gameState.turn
            , Just $ "playerTurn=" <> show gameState.playerTurn
            , Just $ "activePlayer=" <> show gameState.activePlayer
            , Just $ "treasureDraw=" <> show (length drawPile)
            , Just $ "treasures=" <> treasureMarkerSummary
            , boardSummaryPart
            , Just $ "gameOver=" <> show gameState.gameOver
            , Just $ "seed=" <> show gameState.seed
            ]
        latestMessagePart
            | gameState.latestMessage == "" = Nothing
            | otherwise = Just gameState.latestMessage
        boardSummaryPart
            | gameState.turn == 1 = Just $ "boardHexes=\n" <> boardHexSummary
            | otherwise = Nothing
        boardHexSummary = Text.intercalate "\n" $ map summarizeHex (toPairs $ getHexMap gameState.terrainBoard)
        summarizeHex (coord, TerrainHex isLargest feature tokens) =
            show (toPair coord) <> " " <> show isLargest <> " " <> show feature <> " " <> show tokens
        treasureMarkerSummary = Text.intercalate ", " $ map summarizeColor allClueColors
        summarizeColor color =
            show color <> ":" <>
                case clueMarkerPositions color of
                    [coord] -> show (toPair coord)
                    coords -> show (length coords)
        clueMarkerPositions color =
            concatMap matchingMarkerPositions (toPairs $ getHexMap gameState.terrainBoard)
            where
                matchingMarkerPositions (coord, TerrainHex _ _ tokens) =
                    replicate (length $ filter (== ClueToken color) tokens) coord
        (drawPile, _) = gameState.treasureDeck


countTokens :: GameState -> (TerrainToken -> Bool) -> Int
countTokens gameState filt
    = length $ concatMap matchingColorTokens (Map.elems $ getHexMap gameState.terrainBoard)
    where
        matchingColorTokens (TerrainHex _ _ tokens) = filter filt tokens
