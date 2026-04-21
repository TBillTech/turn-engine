module Game.CursedTreasure.API
    ( getAllPlayerColors
    , createNewGame
    , censorHiddenInfo
    , enumerateActivePlayerOptions
    , makeMove
    )
where

import Lens.Micro.Platform ((%~), Lens', lens, traversed, _1, _2, Traversal')
import Relude.Extra (bimapF, maximum1)
import Relude.Extra.Map (insert, keys, member, notMember, toPairs, alter)
import System.Random (mkStdGen, uniformR, RandomGen, uniformShuffleList)
import qualified Data.Map.Strict as Map (elemAt, empty,
    union, filter, filterWithKey, restrictKeys, withoutKeys)
import qualified Data.Set as Set (difference, intersection,
    union, difference, empty, filter, elemAt, insert, filter,
    unions)

import Game.CursedTreasure.Types
    ( CensoredGameState (..)
    , ClueCard (..)
    , ClueObject (..)
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
    , AdjacencyPolicy (HorizontalHexAdjacency)
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

createNewGame :: [PlayerDescription] -> Int -> (GameState, [CensoredGameState])
createNewGame players randomSeed = censor newGameState
    where newGameState = createNewGameState players randomSeed
          playerIds = map (\pd -> pd.playerId) players
          censor s = (s, map (censorHiddenInfo s) playerIds)

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
                    , treasureCards = map (const HiddenTreasure) ps.treasureCards
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
            , treasures = []
            , availableJeepMoves = 0
            , availableCluePlays = 0
            , availableRemoveMarkers = 0
            , availablePickupAmulet = 0
            , availableClueCardExchange = 0
            , score = CurrentScore 0
            , treasureCards = [] }

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

addToken :: RandomGen g => Int -> TerrainToken -> (HexMap, g) -> (HexMap, g)
addToken dist t (b, g) | null consideredSet = addToken (dist-1) t (b, g')
                       | otherwise = (alter addIt candidateI b, g')
    where   (i, g') = uniformR (0, length consideredSet -1) g
            candidateI = Set.elemAt i consideredSet
            possibleB = Map.filter possible b
            consideredSet = (fromList . keys $ possibleB) `Set.difference` distanceSet
            distanceSet = Set.unions $ getDistanceSets dist [existingIndexes]
            getDistanceSets 0 sets = sets
            getDistanceSets _ [] = []
            getDistanceSets reps (lastSet:rest) = getDistanceSets (reps-1) $ nextSet:lastSet:rest
                where   nextSet = getBoundarySet (const True) lastSet
            existingIndexes = fromList . keys $ Map.filter hasToken b
            hasToken (TerrainHex _ ts) = toZeroStatue t `elem` map toZeroStatue ts
            toZeroStatue (Statue _) = Statue $ HourHand 0
            toZeroStatue th = th
            possible (TerrainHex f ts)  | (not . null) ts = False
                                        | f == Ocean = False
                                        | f == Lagoon = False
                                        | f == Mountain = False
                                        | t == PalmTree && f == Jungle = False
                                        | otherwise = True
            addIt (Just (TerrainHex f ts)) = Just $ TerrainHex f $ t:ts
            addIt Nothing = Just $ TerrainHex Meadow [t]

addTokens :: RandomGen g => Int -> TerrainToken -> (HexMap, g) -> (HexMap, g)
addTokens 0 _ bG = bG
addTokens n t bG = addTokens (n-1) t $ addToken 4 t bG

getBoundarySet :: (TokenSpaceIndex -> Bool) -> Set TokenSpaceIndex -> Set TokenSpaceIndex
getBoundarySet filt ts = Set.filter filt $ superKeys `Set.difference` ts
    where superKeys = fromList $ concatMap (adjacentIndices HorizontalHexAdjacency) ts

getBoundary :: ((TokenSpaceIndex, TerrainHex) -> Bool) -> HexMap -> Set TokenSpaceIndex
getBoundary filt m = getBoundarySet setFilter keySet
    where setFilter t = t `notMember` m
          keySet = fromList $ map fst (filter filt $ toPairs m)

isNonOceanTerrain :: (TokenSpaceIndex, TerrainHex) -> Bool
isNonOceanTerrain (_, TerrainHex Ocean _) = False
isNonOceanTerrain _ = True

fillHexOcean :: RandomGen g => (HexMap, g) -> (HexMap, g)
fillHexOcean (m, g) = (Map.union withFirst secondMap, g)
    where firstBoundary = getBoundary isNonOceanTerrain m
          withFirst = foldr (\k m' -> insert k (TerrainHex Ocean []) m') m firstBoundary
          secondBoundary = toList $ getBoundary (const True) withFirst
          secondMap = fromList $ map (, TerrainHex Ocean []) secondBoundary

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
    where unsortedMaps = getSimplyConnectedSets $ Map.filter (\(TerrainHex f' _) -> f' == f) m
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
          convertToLagoon (TerrainHex _ ts) = TerrainHex Lagoon ts
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
                    | tHex == TerrainHex River [] = sift $ first riverFilter shuffledG
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
            usedTHex    | i == 0 = TerrainHex Ocean []
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
            usedTHex    | i == 0 = TerrainHex Ocean []
                        | otherwise = tHex
            mapG' = (insert toAdd usedTHex $ fst mapG, g')

createBoard :: RandomGen g => g -> HexBoard
createBoard g | failed = createBoard $ snd filledTerrainBoardG
              | otherwise = TokenSpace { adjacency = HorizontalHexAdjacency
                                       , tokens = fst fullBoardG}
    where fullBoardG = addTokens 3 PalmTree hutBoardG
          hutBoardG = addTokens 4 Hut allStatueBoardG
          allStatueBoardG = addToken 4 (Statue $ HourHand 1) twoStatueBoardG
          twoStatueBoardG = addToken 4 (Statue $ HourHand 5) oneStatueBoardG
          oneStatueBoardG = addToken 4 (Statue $ HourHand 9) allTerrainBoardG
          (failed, filledTerrainBoardG) = fillInTerrain allTerrainBoardG
          allTerrainBoardG = fillHexOcean beachTerrainBoardG
          beachTerrainBoardG = growTerrainSeeds (CoastalGrowth, [6,5,4,3]) (TerrainHex Beach []) meadowTerrainBoardG
          meadowTerrainBoardG = growTerrainSeeds (RandomGrowth, [10,6,4]) (TerrainHex Meadow []) jungleTerrainBoardG
          jungleTerrainBoardG = growTerrainSeeds (RandomGrowth, [16,10,6]) (TerrainHex Jungle []) mountainTerrainBoardG
          mountainTerrainBoardG = growTerrainSeeds (RandomGrowth, [12,8,6]) (TerrainHex Mountain []) riverTerrainBoardG
          riverTerrainBoardG = growTerrainSeeds (RiverGrowth, [5,3,3]) (TerrainHex River []) lagoonTerrainBoardG
          lagoonTerrainBoardG = growTerrainSeeds (RandomGrowth, [10,6,4]) (TerrainHex Lagoon []) emptyHexMapG
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
                , treasureBoards = []
                , raisingTreasure = Nothing
                , latestMessage = "Player " <> show firstPlayerName <> " Turn"
                , gameOver = False
                , seed = (randomSeed, 1)
                }
            validColors = all ((`elem` allPlayerColors) . (\pd -> pd.playerColor)) playerDs
            newPlayers = map createNewPlayer playerDs
            firstPlayer = maybe (PlayerId 0) (\(p, _) -> p.player.playerId) $ uncons newPlayers
            firstPlayerName = maybe "Missing" (\(p, _) -> p.player.playerName) $ uncons newPlayers
            g = mkStdGen randomSeed
            (randomizedClues, gT) = uniformShuffleList allClues g
            (stackedTreasure, gB) = stackTreasure gT
            board = createBoard gB
            startCards = if length playerDs == 2 then 6 else 4
            dealClues s = dealStartClues (map (.player) s.players) s
            dealStartClues [] s = s
            dealStartClues (pd:restPlayers) s = dealStartClues restPlayers $
                dealCluesToPlayer startCards pd.playerId s
            dealCluesToPlayer 0 _ s = s
            dealCluesToPlayer n pId s = dealCluesToPlayer (n-1) pId $
                s & messageL %~ eUpd & clueDeckL %~ deckUpd & playerL pId %~ psUpd
                where (eUpd, deckUpd, psUpd) = getUpds $ dealClueCardToPlayer s.clueDeck (findPlayer pId s)
                      getUpds (Left e) = ((e <>), id, id)
                      getUpds (Right (dUpd, pUpd)) = (id, dUpd, pUpd)

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

dealClueCardToPlayer :: Deck ClueCard -> Either Text PlayerState
    -> Either Text (Deck ClueCard -> Deck ClueCard, PlayerState -> PlayerState)
dealClueCardToPlayer _ (Left e) = Left e
dealClueCardToPlayer (draw, _) (Right pS)
    = bimapF first (cluesL %~) (moveCard Nothing draw pS.clues)

moveCard :: (Eq a, Show a) => Maybe a -> [a] -> [a]
    -> Either Text ([a] -> [a], [a] -> [a])
moveCard Nothing [] _ = Left "Cannot pop card from list"
moveCard Nothing (top:_) _ = Right (drop 1, (top:))
moveCard (Just c) fromL _ =
    case break (== c) fromL of
        (_, []) -> Left $ "Could not find card " <> show c
        (before, _:after) -> Right (const $ before <> after, (c :))

playerL :: PlayerId -> Traversal' GameState PlayerState
playerL wantedId handler gameState =
    (\players -> gameState { players = players }) <$> traverse visit gameState.players
  where
    visit playerState
        | playerState.player.playerId == wantedId = handler playerState
        | otherwise = pure playerState

findPlayer :: PlayerId -> GameState -> Either Text PlayerState
findPlayer wantedId gameState = maybeToRight ("Count not find player " <> show wantedId) $
    find ((wantedId == ) . (.player.playerId)) gameState.players

clueDeckL :: Lens' GameState ([ClueCard], [ClueCard])
clueDeckL = lens (.clueDeck) (\gameState clueDeck -> gameState { clueDeck = clueDeck })

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
hasToken t _ (TerrainHex _ ts) = t `elem` ts

findFirstToken :: TerrainToken -> Map TokenSpaceIndex TerrainHex -> Maybe (TokenSpaceIndex, TerrainHex)
findFirstToken t = (head <$>) . mFindLocationList (hasToken t)

raiseTreasureCase :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
raiseTreasureCase player gS (GameModeNominal, moves) =
    (GameModeNominal, (RaiseTreasure <$> validColors) ++ moves)
    where   validColors = mapMaybe mColor $ filter exactlyOne possibleColorTokens
            jeepHex = findFirstToken (PlayerJeep player.player.playerId) gS.terrainBoard.tokens
            tokenList = maybe [] (\(_, TerrainHex _ ts) -> ts) jeepHex
            possibleColorTokens = filter (isJust . mColor) tokenList
            mColor (ClueToken t) = Just t
            mColor _ = Nothing
            exactlyOne t = ((1 ==) . length) $ findLocations (hasToken t) gS.terrainBoard.tokens
raiseTreasureCase _ _ gMoves = gMoves

pickupAmuletCase :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
pickupAmuletCase player gS (GameModeNominal, moves) = (GameModeNominal, possibleAmulet moves)
    where   jeepHex = findFirstToken (PlayerJeep player.player.playerId) gS.terrainBoard.tokens
            tokenList = maybe [] (\(_, TerrainHex _ ts) -> ts) jeepHex
            possibleAmulet | Amulet `elem` tokenList = (PickupAmulet:)
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

enumeratePossibleCluePlays :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
enumeratePossibleCluePlays player gS (mode, moves) = undefined

enumeratePossibleJeepMoves :: PlayerState -> GameState -> (GameMode, [PlayerMove]) -> (GameMode, [PlayerMove])
enumeratePossibleJeepMoves player gS (mode, moves) = undefined

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



makeMove :: GameState -> PlayerMove -> (GameState, [CensoredGameState])
makeMove = undefined
