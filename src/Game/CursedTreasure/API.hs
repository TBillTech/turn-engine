module Game.CursedTreasure.API
    ( getAllPlayerColors
    , createNewGame
    , censorHiddenInfo
    , enumerateActivePlayerOptions
    , makeMove
    )
where

import Lens.Micro.Platform ((%~), Lens', lens, Traversal')
import Relude.Extra (bimapF, maximum1)
import Relude.Extra.Map (insert, keys, member, notMember, toPairs, alter)
import System.Random (mkStdGen, uniformR, RandomGen, uniformShuffleList, splitGen, StdGen)
import qualified Data.Map.Strict as Map (elemAt, empty,
    union, filter, filterWithKey, restrictKeys, withoutKeys)
import qualified Data.Set as Set (difference, intersection,
    union, difference, empty, filter, elemAt, insert, filter,
    unions)

import Game.CursedTreasure.Types
    ( CensoredGameState (..)
    , ClueCard (..)
    , ClueObject (..)
    , ClueColor (..)
    , ClueBoard
    , Feature (..)
    , GameState (..)
    , PlayerDescription (..)
    , PlayerId (..)
    , PlayerState (..)
    , RaisingTreasureState (..)
    , TerrainToken (..)
    , TreasureCard (..)
    , PlayerMove
    , TerrainHex (..)
    , allPlayerColors
    , allClueColors
    , Score (..)
    , HexBoard
    , allFeatures
    , Deck
    , TreasureBoard
    , PlayerMove (..)
    , GameMode (..)
    )
import Game.Core.Primitives
    ( TokenSpace (..)
    , HourHand (..)
    , AdjacencyPolicy (..)
    , TokenSpaceIndex (..)
    , adjacentIndices
    , isAdjacentIndex
    , policyFourSquareDistance)

getAllPlayerColors :: [PlayerDescription]
getAllPlayerColors = zipWith mkPlayerDescription [1 ..] allPlayerColors
    where
        mkPlayerDescription i c =
            PlayerDescription
                { playerId = PlayerId i
                , playerName = "Player " <> show i
                , playerAI = "Unassigned"
                , playerColor = c
                }

censorGame :: GameState -> (GameState, [CensoredGameState])
censorGame = censor
    where   playerIds s = map (\pd -> pd.player.playerId) s.players
            censor s = (s, map (censorHiddenInfo s) $ playerIds s)


createNewGame :: [PlayerDescription] -> Int -> (GameState, [CensoredGameState])
createNewGame players randomSeed = censorGame newGameState
    where newGameState = createNewGameState players randomSeed

-- | censorHiddenInfo censors (Converts to "Hidden" states) the following:
-- | * Other players clues
-- | * Other players treasureCards
-- | * The clueDeck draw pile (fst of clueDeck)
-- | * The treasureDeck draw pile (fst of treasureDeck)
-- | * The treasureChest of the RaisingTreasuresState
censorHiddenInfo :: GameState -> PlayerId -> CensoredGameState
censorHiddenInfo g viewerId =
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

        showDeckOpt True (t:rest) = t:map (const HiddenTreasure) rest
        showDeckOpt _ d = map (const HiddenTreasure) d

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

mkWithinStepsOfClues :: ClueObject -> [ClueCard]
mkWithinStepsOfClues co = [WithinStepsOf 1 co
                          ,WithinStepsOf 2 co
                          ,NotWithinStepsOf 1 co
                          ,NotWithinStepsOf 2 co]

mkIsOnClues :: ClueObject -> [ClueCard]
mkIsOnClues co = [IsOn co, IsNotOn co]

mkWithinAndOnClues :: ClueObject -> [ClueCard]
mkWithinAndOnClues co = mkWithinStepsOfClues co ++ mkIsOnClues co

mkFeatureClues :: Feature -> [ClueCard]
mkFeatureClues Ocean = mkWithinStepsOfClues $ FeatureClue False Ocean
mkFeatureClues f = mkWithinAndOnClues (FeatureClue True f) ++
                   mkWithinAndOnClues (FeatureClue False f)

mkTokenClues :: TerrainToken -> [ClueCard]
mkTokenClues t = mkWithinStepsOfClues $ TokenClue t

allClues :: [ClueCard]
allClues = featureClues ++ terrainClues ++ statueClues
    where featureClues = concatMap mkFeatureClues allFeatures
          terrainClues = concatMap mkTokenClues [PalmTree, Hut]
          statueClues = mkWithinStepsOfClues StatueClue

allTreasures :: [TreasureCard]
allTreasures = map Treasure $ replicate 3 6 ++
                              replicate 6 5 ++
                              replicate 11 4 ++
                              replicate 9 3 ++
                              replicate 10 2

stackTreasure :: RandomGen g => g -> ([TreasureCard], g)
stackTreasure g = (twelve ++ bottom, g'')
    where (randomTreasures, g') = uniformShuffleList allTreasures g
          (twelve, rest) = splitAt 12 randomTreasures
          (bottom, g'') = uniformShuffleList (Curse:Curse:rest) g'

type HexMap = Map TokenSpaceIndex TerrainHex

distanceSet :: Int -> Set TokenSpaceIndex -> Set TokenSpaceIndex
distanceSet dist existingIndexes = Set.unions $ getDistanceSets dist [existingIndexes]
    where
        getDistanceSets 0 sets = sets
        getDistanceSets _ [] = []
        getDistanceSets reps (lastSet:rest) = getDistanceSets (reps-1) $ nextSet:lastSet:rest
            where   nextSet = getBoundarySet (const True) lastSet


addToken :: RandomGen g => Int -> TerrainToken -> (HexMap, g) -> (HexMap, g)
addToken dist t (b, g) | null consideredSet = addToken (dist-1) t (b, g')
                       | otherwise = (alter addIt candidateI b, g')
    where   (i, g') = uniformR (0, length consideredSet -1) g
            candidateI = Set.elemAt i consideredSet
            possibleB = Map.filter possible b
            consideredSet = (fromList . keys $ possibleB) `Set.difference` distSet
            distSet = distanceSet dist existingIndexes
            existingIndexes = fromList . keys $ Map.filter hasT b
            hasT (TerrainHex _ _ ts) = toZeroStatue t `elem` map toZeroStatue ts
            toZeroStatue (Statue _) = Statue $ HourHand 0
            toZeroStatue th = th
            possible (TerrainHex _ f ts)  | (not . null) ts = False
                                          | f == Ocean = False
                                          | f == Lagoon = False
                                          | f == Mountain = False
                                          | t == PalmTree && f == Jungle = False
                                          | otherwise = True
            addIt (Just (TerrainHex isL f ts)) = Just $ TerrainHex isL f $ t:ts
            addIt Nothing = Just $ TerrainHex False Meadow [t]

addTokens :: RandomGen g => Int -> TerrainToken -> (HexMap, g) -> (HexMap, g)
addTokens 0 _ bG = bG
addTokens n t bG = addTokens (n-1) t $ addToken 4 t bG

getBoundarySet :: (TokenSpaceIndex -> Bool) -> Set TokenSpaceIndex -> Set TokenSpaceIndex
getBoundarySet filt ts = Set.filter filt $ superKeys `Set.difference` ts
    where superKeys = fromList $ concatMap (adjacentIndices HorizontalHexAdjacency) ts

getBoundary :: (TokenSpaceIndex -> TerrainHex -> Bool) -> HexMap -> Set TokenSpaceIndex
getBoundary filt m = getBoundarySet setFilter keySet
    where setFilter t = t `notMember` m
          keySet = fromList $ map fst (filter (uncurry filt) $ toPairs m)

isNonOceanTerrain :: TokenSpaceIndex -> TerrainHex -> Bool
isNonOceanTerrain _ (TerrainHex _ Ocean _) = False
isNonOceanTerrain _ _ = True

fillHexOcean :: RandomGen g => (HexMap, g) -> (HexMap, g)
fillHexOcean (m, g) = (Map.union withFirst secondMap, g)
    where firstBoundary = getBoundary isNonOceanTerrain m
          withFirst = foldr (\k m' -> insert k (TerrainHex True Ocean []) m') m firstBoundary
          secondBoundary = toList $ getBoundary (const . const True) withFirst
          secondMap = fromList $ map (, TerrainHex True Ocean []) secondBoundary

splitNeighbors :: Set TokenSpaceIndex -> [TokenSpaceIndex] -> Set TokenSpaceIndex
    -> (Set TokenSpaceIndex, [TokenSpaceIndex], Set TokenSpaceIndex)
splitNeighbors c ks s | null keyIntersection = (c', [], s')
                      | otherwise = splitNeighbors c' (toList keyIntersection) s'
    where s' = Set.difference s keyIntersection
          c' = c `Set.union` fromList ks
          keyCandidates = fromList $ concatMap (adjacentIndices HorizontalHexAdjacency) ks
          keyIntersection = Set.intersection keyCandidates s

getSimplyConnectedSet :: HexMap -> (HexMap, HexMap)
getSimplyConnectedSet m | null m = (m, m)
                        | otherwise = (connectedMap, remainderMap)
    where th0 = Map.elemAt 0 m
          keySet = fromList (keys m)
          (connectedSet, _, _) = splitNeighbors Set.empty [fst th0] keySet
          connectedMap = Map.restrictKeys m connectedSet
          remainderMap = Map.withoutKeys m connectedSet


getSimplyConnectedSets :: HexMap -> [HexMap]
getSimplyConnectedSets m | null m = []
                         | otherwise = found:getSimplyConnectedSets m'
    where (found, m') = getSimplyConnectedSet m

getFeatureConnectedSets :: HexMap -> Feature -> (Feature, [HexMap])
getFeatureConnectedSets m f = (f, sortedMaps)
    where unsortedMaps = getSimplyConnectedSets $ Map.filter (\(TerrainHex _ f' _) -> f' == f) m
          sortedMaps = sortBy (\m1 m2 -> length m2 `compare` length m1) unsortedMaps

getConnectedSets :: HexMap -> [(Feature, [HexMap])]
getConnectedSets m = map (getFeatureConnectedSets m) allFeatures

isValidTerrainBoard :: HexMap -> Bool
isValidTerrainBoard m = all isValidFeature $ getConnectedSets m
    where isValidFeature (Ocean, fs) = length fs == 1
          isValidFeature (_, []) = False
          isValidFeature (_, [fg0]) = not (null fg0)
          isValidFeature (_, fg0:fg1:_) = length fg0 > length fg1

fillInTerrain :: RandomGen g => (HexMap, g) -> (Bool, (HexMap, g))
fillInTerrain (m, g) | contiguous = (True, (m, g))
                     | solved = (True, (solution, g))
                     | otherwise = (False, (m, g))
    where contiguous = isOcean && length oceans == 1
          solved = isOcean && isValidTerrainBoard solution
          solution = toLagoon isolated m
          connectedSets = getConnectedSets m
          mOcean = fst <$> uncons connectedSets
          isOcean = maybe False ((== Ocean) . fst) mOcean
          oceans = maybe [Map.empty] snd mOcean
          isolated = map (convertToLagoon <$>) $ drop 1 oceans
          convertToLagoon (TerrainHex isL _ ts) = TerrainHex isL Lagoon ts
          toLagoon [] m' = m'
          toLagoon (iso:isos) m' = toLagoon isos $ Map.union iso m'

addTerrainSeeds :: RandomGen g => Int -> TerrainHex -> (HexMap, g)
    -> ([TokenSpaceIndex], (HexMap, g))
addTerrainSeeds count tHex (m, g) = (fst seedsG, first (Map.union m) mapG)
    where   boundary = getBoundary isNonOceanTerrain m
            oceanBound = m `Map.restrictKeys` boundary
            available = Set.difference boundary $ fromList $ keys oceanBound
            shuffledG = uniformShuffleList (toList available) g
            mapG = first (fromList . map (, tHex)) seedsG
            seedsG  | length (fst shuffledG) < count = first initialize shuffledG
                    | tHex == TerrainHex False River [] = sift $ first riverFilter shuffledG
                    | otherwise = sift $ first adjFilter shuffledG
            initialize :: [TokenSpaceIndex] -> [TokenSpaceIndex]
            initialize sds  | length sds >= count = take count sds
                            | otherwise = initialize $ newI:sds
                where   newI = findClear sds (shifts $ length sds) (lastIndex sds)
                        lastIndex [] = TokenSpace2DIndex 0 0
                        lastIndex (t:_) = t
                        shifts i | i `mod` 3 == 0 = ((+3), (+0))
                                 | i `mod` 3 == 1 = ((+ (-4)), (+2))
                                 | otherwise = ((+0), (+ (-4)))
            findClear _ (app0, app1) (TokenSpaceTextIndex t)
                = TokenSpaceTextIndex $ t <> show (app0 . app1 $ 1)
            findClear sds (app0, app1) (TokenSpaceIntIndex i)
                = findClear' sds (app0, app1) (TokenSpaceIntIndex (app0 . app1 $ i))
            findClear sds (app0, app1) (TokenSpace2DIndex i j)
                = findClear' sds (app0, app1) (TokenSpace2DIndex (app0 i) (app1 j))
            findClear' sds (app0, app1) t
                | t `member` m = findClear sds (app0, app1) t
                | t `elem` sds = findClear sds (app0, app1) t
                | otherwise = t

            adjnt = isAdjacentIndex HorizontalHexAdjacency
            adjFilter [] = []
            adjFilter (t0:rest) = t0:filter (not . adjnt t0) (adjFilter rest)
            sift (sds, rg)  | length sds >= count = (take count sds, rg)
                            | otherwise = sift (adjFilter shuffled', rg')
                where (shuffled', rg') = uniformShuffleList (fst shuffledG) rg

            riverFilter = adjFilter . filter distFilter
                where distFilter = (((4*4+4*4)*4) <) . sumSqrs
                      sumSqrs = maybe 0 (uncurry (+)) .
                            policyFourSquareDistance HorizontalHexAdjacency
                            (TokenSpace2DIndex 0 0)

data GrowthRule = RandomGrowth | RiverGrowth | CoastalGrowth

growTerrainSeeds :: RandomGen g => (GrowthRule, [Int]) -> TerrainHex -> (HexMap, g) -> (HexMap, g)
growTerrainSeeds (rule, sizes) tHex mg = grownG
    where   (seeds, withSeedsG) = addTerrainSeeds (length sizes) tHex mg
            seedSets = zip sizes $ map one seeds
            grownG = foldr (growTerrainSeed rule tHex) withSeedsG seedSets

growTerrainSeed :: RandomGen g => GrowthRule -> TerrainHex -> (Int, Set TokenSpaceIndex)
    -> (HexMap, g) -> (HexMap, g)
growTerrainSeed RandomGrowth _ (0, _) mapG = mapG
growTerrainSeed RandomGrowth tHex (i, territory) mapG
    = growTerrainSeed RandomGrowth tHex (i-1, territory') mapG'
    where   territoryBound = getBoundarySet (`notMember` fst mapG) territory
            (rI, g') = uniformR (0, length territoryBound - 1) $ snd mapG
            toAdd = Set.elemAt rI territoryBound
            territory' = Set.insert toAdd territory
            mapG' = (insert toAdd tHex $ fst mapG, g')
growTerrainSeed RiverGrowth _ (-1, _) mapG = mapG
growTerrainSeed RiverGrowth tHex (i, territory) mapG
    = growTerrainSeed RiverGrowth tHex (i-1, territory') mapG'
    where   fullTerritoryBound = toList $ getBoundarySet (`notMember` fst mapG) territory
            distTo = maybe 0 (uncurry (+)) .
                policyFourSquareDistance HorizontalHexAdjacency
                (TokenSpace2DIndex 0 0)
            territoryDists = zip (map distTo fullTerritoryBound) fullTerritoryBound
            maxDist = maybe 0 maximum1 (nonEmpty (map fst territoryDists))
            territoryBound = fromList $ map snd $ filter ((maxDist == ) . fst) territoryDists
            (rI, g') = uniformR (0, length territoryBound - 1) $ snd mapG
            toAdd = Set.elemAt rI territoryBound
            territory' = Set.insert toAdd territory
            usedTHex    | i == 0 = TerrainHex True Ocean []
                        | otherwise = tHex
            mapG' = (insert toAdd usedTHex $ fst mapG, g')
growTerrainSeed CoastalGrowth _ (-1, _) mapG = mapG
growTerrainSeed CoastalGrowth tHex (i, territory) mapG
    = growTerrainSeed CoastalGrowth tHex (i-1, territory') mapG'
    where   fullTerritoryBound = getBoundarySet (`notMember` fst mapG) territory
            territoryBound = Set.filter keepEnds fullTerritoryBound
            keepEnds t  | length tIntersect > 1 = False
                        | otherwise = True
                where   tBorder = fromList $ adjacentIndices HorizontalHexAdjacency t
                        tIntersect = Set.intersection territory tBorder
            (rI, g') = uniformR (0, length territoryBound - 1) $ snd mapG
            toAdd = Set.elemAt rI territoryBound
            territory' = Set.insert toAdd territory
            usedTHex    | i == 0 = TerrainHex True Ocean []
                        | otherwise = tHex
            mapG' = (insert toAdd usedTHex $ fst mapG, g')

createBoard :: RandomGen g => g -> HexBoard
createBoard g | failed = createBoard $ snd filledTerrainBoardG
              | otherwise = TokenSpace { adjacency = HorizontalHexAdjacency
                                       , tokens = setLargest connectedSets fullBoard}
    where   connectedSets = getConnectedSets fullBoard
            setLargest [] board = board
            setLargest ((_, []):rest) board = setLargest rest board
            setLargest ((_, large:_):rest) board = setLargest rest $
                foldr (alter toLargest) board (keys large)
            toLargest (Just (TerrainHex _ f ts)) = Just (TerrainHex True f ts)
            toLargest _ = Nothing
            fullBoard = fst $ addTokens 3 PalmTree hutBoardG
            hutBoardG = addTokens 4 Hut allStatueBoardG
            allStatueBoardG = addToken 4 (Statue $ HourHand 1) twoStatueBoardG
            twoStatueBoardG = addToken 4 (Statue $ HourHand 5) oneStatueBoardG
            oneStatueBoardG = addToken 4 (Statue $ HourHand 9) filledTerrainBoardG
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


createNewGameState :: [PlayerDescription] -> Int -> GameState
createNewGameState playerDs randomSeed
    | not validColors = initState & messageL %~ const "Player Colors Invalid"
    | length playerDs <= 2 = initState & messageL %~ const "Game must be played with at least 2 players"
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
            firstPlayer = maybe (PlayerId 0) (\(p, _) -> p.player.playerId) $ uncons newPlayers
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

_eitherUpdates :: Semigroup a1 => Either a1 (a2 -> a2) -> (a1 -> a1, a2 -> a2)
_eitherUpdates (Left e) = ((e <>), id)
_eitherUpdates (Right upd) = (id, upd)

_eitherUpdates2 :: Semigroup a1 => Either a1 (a2 -> a2, a3 -> a3)
    -> (a1 -> a1, a2 -> a2, a3 -> a3)
_eitherUpdates2 (Left e) = ((e <>), id, id)
_eitherUpdates2 (Right (updA, updB)) = (id, updA, updB)

_eitherUpdates3 :: Semigroup a1 => Either a1 (a2 -> a2, a3 -> a3, a4 -> a4)
    -> (a1 -> a1, a2 -> a2, a3 -> a3, a4 -> a4)
_eitherUpdates3 (Left e) = ((e <>), id, id, id)
_eitherUpdates3 (Right (updA, updB, updC)) = (id, updA, updB, updC)

_eitherUpdates4 :: Semigroup a1 => Either a1 (a2 -> a2, a3 -> a3, a4 -> a4, a5 -> a5)
    -> (a1 -> a1, a2 -> a2, a3 -> a3, a4 -> a4, a5 -> a5)
_eitherUpdates4 (Left e) = ((e <>), id, id, id, id)
_eitherUpdates4 (Right (updA, updB, updC, updD)) = (id, updA, updB, updC, updD)

newTreasureBoard :: ClueColor -> TreasureBoard
newTreasureBoard c = (c, [])

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

nextPlayer :: PlayerId -> GameState -> PlayerId
nextPlayer currentPlayer gameState = fromMaybe (fromMaybe currentPlayer mFirstPlayerId) mNext
    where   (mFirstPlayerId, _, mNext) = foldr foldfn (Nothing, False, Nothing) gameState.players
            isCurrent player = player.player.playerId == currentPlayer
            foldfn _ finished@(_, _, Just _) = finished
            foldfn player (Nothing, _, _) = (Just player.player.playerId, isCurrent player, Nothing)
            foldfn player (Just fp, False, _) = (Just fp, isCurrent player, Nothing)
            foldfn player (Just fp, True, _) = (Just fp, False, Just player.player.playerId)

setNotPlayerTurn :: PlayerState -> PlayerState
setNotPlayerTurn ps = ps    { availableJeepMoves = 0
                            , availableCluePlays = 0
                            , availableRemoveMarkers = 0
                            , availablePickupAmulet = 0
                            , availableClueCardExchange = 0 }

setPlayerTurn :: PlayerState -> PlayerState
setPlayerTurn ps = ps   { availableJeepMoves = 3
                        , availableCluePlays = 1
                        , availableRemoveMarkers = 0
                        , availablePickupAmulet = 1
                        , availableClueCardExchange = 1 }

mkStdGenN :: Int -> Int -> (StdGen, StdGen)
mkStdGenN seed 0 = splitGen $ mkStdGen seed
mkStdGenN seed n = splitGen (snd $ mkStdGenN seed (n-1))

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

moveCards :: (Eq a, Show a, Eq b, Show b) => Int -> Maybe a -> (a -> b) -> [a] -> [b]
    -> Either Text ([a] -> [a], [b] -> [b])
moveCards 0 _ _ _ _ = Right (id, id)
moveCards n mSpecific convert from to = (uncurry bimap . bimap (.) (.) <$> eMoveCard)
    <*> moveCards (n-1) mSpecific convert from to
    where   eMoveCard = moveCard mSpecific convert from to

moveCard :: (Eq a, Show a, Eq b, Show b) => Maybe a -> (a -> b) -> [a] -> [b]
    -> Either Text ([a] -> [a], [b] -> [b])
moveCard Nothing _  [] _ = Left "Cannot pop card from list"
moveCard Nothing toB (top:_) _ = Right (drop 1, (toB top:))
moveCard (Just c) toB fromL _ =
    case break (== c) fromL of
        (_, []) -> Left $ "Could not find card " <> show c
        (before, _:after) -> Right (const $ before <> after, (toB c :))

playerL :: PlayerId -> Traversal' GameState PlayerState
playerL wantedId handler gameState =
    (\players -> gameState { players = players }) <$> traverse visit gameState.players
  where
    visit playerState
        | playerState.player.playerId == wantedId = handler playerState
        | otherwise = pure playerState

clueDeckL :: Lens' GameState ([ClueCard], [ClueCard])
clueDeckL = lens (.clueDeck) (\gameState clueDeck -> gameState { clueDeck = clueDeck })

treasureDeckL :: Lens' GameState ([TreasureCard], [TreasureCard])
treasureDeckL = lens (.treasureDeck) (\gameState treasureDeck -> gameState { treasureDeck = treasureDeck })

messageL :: Lens' GameState Text
messageL = lens (.latestMessage) (\gameState message -> gameState { latestMessage = message })

playerTurnL :: Lens' GameState PlayerId
playerTurnL = lens (.playerTurn) (\gameState playerId -> gameState { playerTurn = playerId })

playerActiveL :: Lens' GameState PlayerId
playerActiveL = lens (.activePlayer) (\gameState playerId -> gameState { activePlayer = playerId })

playersL :: Lens' GameState [PlayerState]
playersL = lens (.players) (\gameState players -> gameState { players = players })

turnL :: Lens' GameState Int
turnL = lens (.turn) (\gameState t -> gameState { turn = t })

cluesL :: Lens' PlayerState [ClueCard]
cluesL = lens (.clues) (\playerState clues -> playerState { clues = clues })

boardL :: Lens' GameState HexMap
boardL = lens (.terrainBoard.tokens)
    (\gameState board -> gameState { terrainBoard = gameState.terrainBoard { tokens = board}})

seedL :: Lens' GameState (Int, Int)
seedL = lens (.seed)
    (\gameState seed -> gameState { seed = seed})

treasureL :: ClueColor -> Traversal' GameState ClueBoard
treasureL wantedColor handler gameState =
    (\treasures -> gameState { treasureBoards = treasures }) <$> traverse visit gameState.treasureBoards
  where
    visit treasure@(color, board)
        | color == wantedColor = (color, ) <$> handler board
        | otherwise = pure treasure

activePlayerL :: Lens' GameState PlayerId
activePlayerL = lens (.activePlayer)
    (\gameState activePlayer -> gameState { activePlayer = activePlayer})

raiseTreasureL :: Lens' GameState (Maybe RaisingTreasureState)
raiseTreasureL = lens (.raisingTreasure)
    (\gameState raisingTreasure -> gameState { raisingTreasure = raisingTreasure})

findPlayer :: PlayerId -> GameState -> Either Text PlayerState
findPlayer wantedId gameState = maybeToRight ("Could not find player " <> show wantedId) $
    find ((wantedId == ) . (.player.playerId)) gameState.players

findTreasure :: ClueColor -> GameState -> Either Text ClueBoard
findTreasure color gameState = maybeToRight ("Could not find treasure " <> show color) $
    snd <$> find ((color ==) . fst) gameState.treasureBoards

passTurnOption :: (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
passTurnOption (GameModeNominal, moves) = (GameModeNominal, PassTurn:moves)
passTurnOption modeMoves = modeMoves

isRaisingTreasure :: GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
isRaisingTreasure gS (mode, moves) = case gS.raisingTreasure of
    Nothing -> (mode, moves)
    Just treasureState -> if not (null treasureState.rtViewing)
        then (GameModeRaisingTreasureView treasureState, moves)
        else (GameModeRaisingTreasureChoice treasureState, moves)

raisingTreasureChoiceCase :: PlayerState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
raisingTreasureChoiceCase player (GameModeRaisingTreasureChoice ts, moves)
    = (GameModeRaisingTreasureChoice ts, pushOptions moves)
    where   pushOptions = case (ts.rtTreasureChest, player.amulets > 0) of
                ((Curse:_, _), True) -> (RaisingTreasureAcceptCurse:) . (RaisingTreasureWardCurse:)
                ((Curse:_, _), False) -> (RaisingTreasureAcceptCurse:)
                _ -> (RaisingTreasurePass:) . (RaisingTreasureTake:)
raisingTreasureChoiceCase _ gMoves = gMoves

raisingTreasureViewCase :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
raisingTreasureViewCase _ _ (GameModeRaisingTreasureView ts, moves) =
    (GameModeRaisingTreasureView ts, RaisingTreasurePass:moves)
raisingTreasureViewCase _ _ gMoves = gMoves

findLocations :: (TokenSpaceIndex -> TerrainHex -> Bool) -> Map TokenSpaceIndex TerrainHex
    -> Map TokenSpaceIndex TerrainHex
findLocations = Map.filterWithKey

mFindLocationList :: (TokenSpaceIndex -> TerrainHex -> Bool) -> Map TokenSpaceIndex TerrainHex
    -> Maybe (NonEmpty (TokenSpaceIndex, TerrainHex))
mFindLocationList filt board = nonEmpty . toPairs $ findLocations filt board

hasToken :: TerrainToken -> TokenSpaceIndex -> TerrainHex -> Bool
hasToken t _ (TerrainHex _ _ ts) = t `elem` ts

findFirstToken :: TerrainToken -> Map TokenSpaceIndex TerrainHex -> Maybe (TokenSpaceIndex, TerrainHex)
findFirstToken t = (head <$>) . mFindLocationList (hasToken t)

raiseTreasureCase :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
raiseTreasureCase player gS (GameModeNominal, moves) =
    (GameModeNominal, (RaiseTreasure <$> validColors) ++ moves)
    where   validColors = mapMaybe mColor $ filter exactlyOne possibleColorTokens
            jeepHex = findFirstToken (PlayerJeep player.player.playerId) gS.terrainBoard.tokens
            tokenList = maybe [] (\(_, TerrainHex _ _ ts) -> ts) jeepHex
            possibleColorTokens = filter (isJust . mColor) tokenList
            mColor (ClueToken t) = Just t
            mColor _ = Nothing
            exactlyOne t = ((1 ==) . length) $ findLocations (hasToken t) gS.terrainBoard.tokens
raiseTreasureCase _ _ gMoves = gMoves

pickupAmuletCase :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
pickupAmuletCase player gS (GameModeNominal, moves) = (GameModeNominal, possibleAmulet moves)
    where   jeepHex = findFirstToken (PlayerJeep player.player.playerId) gS.terrainBoard.tokens
            tokenList = maybe [] (\(_, TerrainHex _ _ ts) -> ts) jeepHex
            possibleAmulet | player.availablePickupAmulet > 0 && Amulet `elem` tokenList = (PickupAmulet:)
                           | otherwise = id
pickupAmuletCase _ _ gMoves = gMoves

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
            colorToLocations = uncurry findLocations . (, gS.terrainBoard.tokens) . hasToken . ClueToken
            colorLocations = filter ((> 1) . length . snd) $ zip allClueColors (map colorToLocations allClueColors)
            possibleRemoveMarkers = concatMap (asRemovePlay . second keys) colorLocations
            asRemovePlay (c, locs) = mapMaybe ((uncurry (UseAmuletRemoveSiteMarker c) <$>) . locToTuple) locs
            locToTuple (TokenSpace2DIndex x y) = Just (x, y)
            locToTuple _ = Nothing
useAmuletCase _ _ gMoves = gMoves

matchObject :: ClueObject -> TokenSpaceIndex -> TerrainHex -> Bool
matchObject (FeatureClue True clueF) _ (TerrainHex isL tF _) = isL && clueF == tF
matchObject (FeatureClue _ clueF) _ (TerrainHex _ tF _) = clueF == tF
matchObject (TokenClue clueT) _ (TerrainHex _ _ ts) = clueT `elem` ts
matchObject StatueClue _ (TerrainHex _ _ ts) = StatueClue `elem` mapMaybe toStatueClue ts
    where   toStatueClue (Statue _) = Just StatueClue
            toStatueClue _ = Nothing

matchClueCard :: HexMap -> ClueCard -> TokenSpaceIndex -> TerrainHex -> Bool
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

applyClue :: HexMap -> (ClueColor, ClueCard) -> (HexMap, HexMap)
applyClue board (color, card) = (beforeMarkers, afterMarkers)
    where   currentMarkers = findLocations (hasToken $ ClueToken color) board
            beforeMarkers   | null currentMarkers = findLocations isNonOceanTerrain board
                            | otherwise = currentMarkers
            afterMarkers = findLocations (matchCard card) beforeMarkers
            matchCard = matchClueCard board

enumeratePossibleCluePlays :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
enumeratePossibleCluePlays player gS (GameModeNominal, moves) = (GameModeNominal, allOptions ++ moves)
    where   allOptions = concatMap (map toM . filter possible . zip (take 4 allClueColors) . repeat) player.clues
            toM (color, card) = PlayClue color card
            possible cCard = let (before, after) = applyClue gS.terrainBoard.tokens cCard in
                length before > length after && not (null after)
enumeratePossibleCluePlays _ _ gMoves = gMoves

enumeratePossibleJeepMoves :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
enumeratePossibleJeepMoves player gS (GameModeNominal, moves)
    | player.availableJeepMoves == 0 = (GameModeNominal, moves)
    | null oneLeg = (GameModeMustMoveJeep, mapMaybe toMove unconstrained)
    | otherwise = (GameModeNominal, mapMaybe toMove oneLeg)
    where   connectedSets = concatMap snd (filter ((Ocean /=) . fst) $ getConnectedSets gS.terrainBoard.tokens)
            jeepHex = findFirstToken (PlayerJeep player.player.playerId) gS.terrainBoard.tokens
            oneLeg = case jeepHex of (Just (k, _)) -> toList $ legSet k
                                     Nothing -> []
            legSet k = Set.unions $ distanceSet 1 (one k):
                map (fromList . keys) (filter (member k) connectedSets)
            unconstrained = map fst $ filter ((\(TerrainHex _ f _) -> f /= Ocean) . snd) $
                toPairs gS.terrainBoard.tokens
            toMove (TokenSpace2DIndex i j) = Just $ MoveJeep i j
            toMove _ = Nothing
enumeratePossibleJeepMoves _ _ gMoves = gMoves


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

enumerateActivePlayerOptions :: GameState -> [PlayerMove]
enumerateActivePlayerOptions gS = enumerateMoves ePlayer
    where   ePlayer = findPlayer gS.activePlayer gS
            enumerateMoves (Left e) = [PlayerMoveError e]
            enumerateMoves (Right player) = case enumeratePlayerOptions player gS of
                (GameModeError e, moves) -> PlayerMoveError e:moves
                (_, moves) -> moves


playClueAndUpdate :: ClueColor -> ClueCard -> PlayerId -> GameState
    -> Either Text (HexMap -> HexMap, PlayerState -> PlayerState, ClueBoard -> ClueBoard)
playClueAndUpdate color card pId gS = eVerifyCard <*> eUpdates
    where   ePlayer = findPlayer pId gS
            eClueBoard = findTreasure color gS
            eVerifyCard = id <$ (findCard =<< ePlayer)
            findCard player = if card `elem` player.clues then Right card
                else Left $ "Could not find " <> show card <> " in player " <> show pId <> " clues."
            boardUpd = snd . flip applyClue (color, card)
            eClues = (.clues) <$> ePlayer
            eCluesAndBoard = (,) <$> eClues <*> eClueBoard
            eCardUpds = uncurry (moveCard (Just card) (pId, )) =<< eCluesAndBoard
            updPlayer playerCardUpd player = player { clues = playerCardUpd player.clues }
            eUpdates = (\(psU, bU) -> (boardUpd, updPlayer psU, bU)) <$> eCardUpds

dealCardsDirect :: Int -> PlayerId -> GameState -> GameState
dealCardsDirect 0 _ gS = gS
dealCardsDirect n pId gS = gS & dealCardsDirect (n-1) pId
    & messageL %~ eDeckUpd & clueDeckL %~ deckUpd & playerL pId %~ psUpd & seedL %~ seedUpd
    where   (eDeckUpd, deckUpd, psUpd, seedUpd) = _eitherUpdates3 $
                dealClueCardToPlayer gS.seed gS.clueDeck (findPlayer pId gS)

alterTokenList :: ([TerrainToken] -> [TerrainToken]) -> Maybe TokenSpaceIndex -> HexMap -> HexMap
alterTokenList updTs (Just k) = alter (updateTokenList <$>) k
    where updateTokenList (TerrainHex isL f ts) = TerrainHex isL f (updTs ts)
alterTokenList _ Nothing = id

playClueDirect :: ClueColor -> ClueCard -> GameState -> GameState
playClueDirect color card gS =
    gS  & dealCard & messageL %~ eUpd & boardL %~ markerUpd & playerL pId %~ playedCardUpd
        & treasureL color %~ treasureBoardUpd
    where   pId = gS.activePlayer
            ePlayClueUpds = playClueAndUpdate color card pId gS
            (eUpd, markerUpd, playedCardUpd, treasureBoardUpd) = _eitherUpdates3 ePlayClueUpds
            dealCard = if isRight ePlayClueUpds then dealCardsDirect 1 pId else id

moveJeepDirect :: Int -> Int -> GameState -> GameState
moveJeepDirect i j gS =
    gS  & boardL %~ addJeep & boardL %~ removeJeep
    where   jeep = PlayerJeep gS.activePlayer
            mJeepHex = fst <$> findFirstToken jeep gS.terrainBoard.tokens
            removeJeep = alterTokenList (filter (/= jeep)) mJeepHex
            addJeep = alterTokenList (jeep:) (Just $ TokenSpace2DIndex i j)

exchangeCardsDirect :: GameState -> GameState
exchangeCardsDirect gS =
    gS  & dealCards & messageL %~ errDiscardUpd & clueDeckL %~ clueUpd
        & playerL pId %~ playerDiscardUpd
    where   pId = gS.activePlayer
            dealCards = if isRight eDiscardUpds then dealCardsDirect 4 pId else id
            (errDiscardUpd, toDiscardUpd, playerCluesUpd) = _eitherUpdates2 eDiscardUpds
            clueUpd = second toDiscardUpd
            playerDiscardUpd player = player { clues = playerCluesUpd player.clues }
            ePlayer = findPlayer pId gS
            eDiscardUpds = moveCards 4 Nothing id (either (const []) (.clues) ePlayer) (snd gS.clueDeck)

dealTreasureCardDirect :: PlayerId -> GameState -> GameState
dealTreasureCardDirect anId s = s & playerL anId %~ updTCards & treasureDeckL %~ updTDeck
    where   updTCards player = player { viewingTreasures = topCard ++ player.viewingTreasures }
            topCard = take 1 $ fst s.treasureDeck
            updTDeck = first $ drop 1

raiseTreasureShuffle :: GameState -> GameState
raiseTreasureShuffle s =
    s & raiseTreasureL %~ (shuffleTCards <$>) & seedL %~ updSeed & raiseTreasureChooseStart
    where   cards = maybe [] (fst . (.rtTreasureChest)) s.raisingTreasure
            updSeed = second (+1)
            rG = fst $ mkStdGenN (snd s.seed) $ fst s.seed
            randomizedTreasures = fst $ uniformShuffleList cards rG
            shuffleTCards rt = rt { rtTreasureChest = (randomizedTreasures, [])}

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
            checkGameWinner gS  | isNothing mCards = gS & playersL %~ (setWinner <$>)
                                | otherwise = gS
            setWinner player = player { score = if CurrentScore maxS == player.score
                                                then WinnerScore maxS else player.score }

raiseTreasureNextChooser :: GameState -> GameState
raiseTreasureNextChooser s  | isNothing mNextPlayerIndex = s & raiseTreasureFinished
                            | otherwise = s & raiseTreasureL %~ (updChooser <$>) & setActivePlayer
    where   mNextPlayerIndex = do
                raising <- s.raisingTreasure
                _ <- nonEmpty (fst raising.rtTreasureChest)
                neOrder <- nonEmpty raising.rtOrder
                let rtPlayerIndex = raising.rtPlayerIndex
                    rtNextPlayerIndex = rtPlayerIndex+1 `mod` length neOrder
                pure rtNextPlayerIndex

            updChooser rt = rt { rtPlayerIndex = fromMaybe 0 mNextPlayerIndex }

raiseTreasureChooseStart :: GameState -> GameState
raiseTreasureChooseStart s  | isNothing mOrder = s & raiseTreasureFinished
                            | otherwise = s & raiseTreasureL %~ (updChooser <$>) & setActivePlayer
    where   mOrder = do
                raising <- s.raisingTreasure
                _ <- nonEmpty (fst raising.rtTreasureChest)
                nonEmpty raising.rtOrder
            updChooser rt = rt { rtPlayerIndex = 0 }

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
                let rtPlayerIndex = raising.rtPlayerIndex
                    rtNextPlayerIndex = rtPlayerIndex+1 `mod` length neOrder
                neAtPlayerIndex <- nonEmpty $ drop rtNextPlayerIndex (toList neOrder)
                pure (head neAtPlayerIndex)

raiseTreasureViewPass :: GameState -> GameState
raiseTreasureViewPass s =
    s   & playerL pId %~ updTCards & treasureDeckL %~ updTDeck & raiseTreasureL %~ (updViewers <$>)
        & setActivePlayer & if lastViewer then raiseTreasureShuffle else id
    where   pId = s.activePlayer
            ePlayer = findPlayer pId s
            playerCards = either (const []) (.viewingTreasures) ePlayer
            updTCards player = player { viewingTreasures = [] }
            updTDeck = first (playerCards ++)
            lastViewer = maybe False ((== 1) . length . (.rtViewing)) mRaising
            mRaising = s.raisingTreasure
            updViewers rt = rt { rtViewing = drop 1 rt.rtViewing}

makeMoveParanoid :: GameState -> PlayerMove -> (GameState, [CensoredGameState])
makeMoveParanoid gS move | move `elem` enumerateActivePlayerOptions gS = makeMoveDirect gS move
                         | otherwise = censorGame $ gS & messageL %~ (e <>)
    where e = "Move " <> show move <> " is not allowed for current player."

makeMove :: GameState -> PlayerMove -> (GameState, [CensoredGameState])
makeMove = makeMoveParanoid

isViewingTreasure :: GameState -> Bool
isViewingTreasure gS = mMode == Just True
    where   mMode = do
                raising <- gS.raisingTreasure
                let isViewing = not . null $ raising.rtViewing
                pure isViewing

isLastChooser :: GameState -> Bool
isLastChooser gS = mMode == Just True
    where   mMode = do
                raising <- gS.raisingTreasure
                let rtPlayerIndex = raising.rtPlayerIndex
                    isLastChoice = rtPlayerIndex == length raising.rtOrder - 1
                pure isLastChoice

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
            playerUsedOptions player = player { availableJeepMoves = player.availableJeepMoves - 1
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
            mJeepHex = findFirstToken jeep gS.terrainBoard.tokens
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
            boardUpd = alterTokenList (filter (/= ClueToken color)) (Just $ TokenSpace2DIndex i j)
makeMoveDirect gS (RaiseTreasure color) = censorGame $
    gS  & treasureDeckL %~ first (drop 1) & initTreasure
        & dealTreasureCards pTurns & treasureL color %~ const [] & boardL %~ (clearColor <$>)
        & nextTurn & setActivePlayer
    where   pId = gS.activePlayer
            pTurns = case filter ((color ==) . fst) gS.treasureBoards of
                [(_, playerIds)] -> pId:map fst playerIds
                _ -> [pId]
            topCard = take 1 $ fst gS.treasureDeck
            initTreasure s = s { raisingTreasure = Just initialRaisingTreasureState }
            initialRaisingTreasureState = RaisingTreasureState
                { rtTreasureChest = (topCard, [])
                , rtOrder = pTurns
                , rtPlayerIndex = 0
                , rtViewing = pTurns}
            clearColor (TerrainHex isL f ts) = TerrainHex isL f (filter (/= ClueToken color) ts)
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
            removePlayerIndex pList
                = take playerIndex pList <> drop (playerIndex+1) pList
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