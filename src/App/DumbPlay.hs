module App.DumbPlay
    ( DumbPlayStats (..)
    , runDumbPlay
    , renderDumbPlayStats
    )
where

import qualified Data.Map.Strict as Map
import Numeric (showFFloat)
import System.Random (StdGen, mkStdGen, uniformR)
import qualified Game.Core.API as Core
import qualified Game.Core.Types as Core
import qualified Game.ArtOfWar.Types as ArtOfWar
import qualified Game.CursedTreasure.API as CursedTreasure
import qualified Game.CursedTreasure.Types as CursedTreasure
import qualified Game.FogOfBattle.Types as FogOfBattle
import qualified Game.RealEstate.Types as RealEstate

import App.CLI (RulesetName (..))

data DumbPlayStats = DumbPlayStats
    { gamesRequested :: Int
    , gamesCompleted :: Int
    , gamesAborted :: Int
    , totalRounds :: Int
    , minRounds :: Maybe Int
    , maxRounds :: Maybe Int
    , totalPlayerTurns :: Int
    , turnsWithClue :: Int
    , turnsWithMove :: Int
    , turnsWithDig :: Int
    , successfulDigTurns :: Int
    , successfulDigIntervals :: Int
    , totalTurnsBetweenSuccessfulDigs :: Int
    , mainPhaseMetrics :: PhaseMetrics
    , raiseTreasurePhaseMetrics :: PhaseMetrics
    , scoreDistribution :: [(Int, Int)]
    , winnerSpreadDistribution :: [(Int, Int)]
    }
    deriving (Show, Eq)

data PhaseMetrics = PhaseMetrics
    { decisionCount :: Int
    , legalMoveCount :: Int
    , forcedDecisionCount :: Int
    , branchingDecisionCount :: Int
    , branchingLegalMoveCount :: Int
    }
    deriving (Show, Eq)

runDumbPlay :: Int -> RulesetName -> IO DumbPlayStats
runDumbPlay gameCount rulesetName =
    foldlM playAndAccumulate emptyStats [0 .. gameCount - 1]
  where
    playAndAccumulate stats index = do
        gameResult <- simulateGame (gameCount == 1) rulesetName (mkStdGen index)
        pure (accumulateResult stats gameResult)
    emptyStats = DumbPlayStats
        { gamesRequested = gameCount
        , gamesCompleted = 0
        , gamesAborted = 0
        , totalRounds = 0
        , minRounds = Nothing
        , maxRounds = Nothing
        , totalPlayerTurns = 0
        , turnsWithClue = 0
        , turnsWithMove = 0
        , turnsWithDig = 0
        , successfulDigTurns = 0
        , successfulDigIntervals = 0
        , totalTurnsBetweenSuccessfulDigs = 0
        , mainPhaseMetrics = emptyPhaseMetrics
        , raiseTreasurePhaseMetrics = emptyPhaseMetrics
        , scoreDistribution = []
        , winnerSpreadDistribution = []
        }

data GameResult
    = GameCompleted CompletedGameStats
    | GameAborted

data CompletedGameStats = CompletedGameStats
    { completedRoundCount :: Int
    , completedPlayerTurnCount :: Int
    , completedSimulationMetrics :: SimulationMetrics
    , completedScores :: [Int]
    , completedWinnerSpread :: Int
    }
    deriving (Show, Eq)

data SimulationMetrics = SimulationMetrics
    { currentTurn :: Maybe TurnTracker
    , pendingSuccessfulDigTreasures :: Maybe Int
    , lastSuccessfulDigTurn :: Maybe Int
    , mainMetrics :: PhaseMetrics
    , raiseTreasureMetrics :: PhaseMetrics
    , completedTurnCount :: Int
    , clueTurnCount :: Int
    , moveTurnCount :: Int
    , digTurnCount :: Int
    , successfulDigTurnCount :: Int
    , successfulDigGapCount :: Int
    , turnsBetweenSuccessfulDigs :: Int
    }
    deriving (Show, Eq)

data TurnTracker = TurnTracker
    { trackedTurnNumber :: Int
    , trackedCommittedMainChoice :: Bool
    , trackedClueTurn :: Bool
    , trackedMoveTurn :: Bool
    , trackedDigTurn :: Bool
    , trackedSuccessfulDig :: Bool
    , trackedPendingDigTreasures :: Maybe Int
    }
    deriving (Show, Eq)

simulateGame :: Bool -> RulesetName -> StdGen -> IO GameResult
simulateGame shouldTrace rulesetName rng0 =
    case createInitialGame rulesetName rng0 of
        Left _ -> pure GameAborted
        Right (gameState, rng1) -> stepGame shouldTrace 10000 emptySimulationMetrics gameState rng1

createInitialGame :: RulesetName -> StdGen -> Either Text (Core.GameState, StdGen)
createInitialGame CursedTreasure rng0 =
    let (selectedPlayerCount, rng1) = chooseInt 2 4 rng0
        setupPlayers = take selectedPlayerCount CursedTreasure.getGameSetupPlayers
        seedValue = 12345
     in case Core.createNewGame (map Core.CursedTreasurePlayerDescription setupPlayers) seedValue of
            Left err -> Left err
            Right (gameState, _) -> Right (gameState, rng1)

stepGame :: Bool -> Int -> SimulationMetrics -> Core.GameState -> StdGen -> IO GameResult
stepGame shouldTrace turnLimit simulationMetrics gameState rng = do
    when shouldTrace $ do
        putTextLn (Core.summary gameState)
        hFlush stdout
    if isGameOver gameState then
        pure (GameCompleted (summarizeCompletedGame gameState (finalizeTrackedTurn True simulationMetrics)))
    else    (if (turnCount >= turnLimit) 
            || null playerMoves then 
                pure GameAborted else 
                (let    candidateMoves = bestMoves gameState playerMoves
                        startedMetrics = beginTrackedTurn gameState simulationMetrics
                        nextTurnMetrics = recordDecisionMetrics gameState playerMoves startedMetrics
                        (moveIndex, nextRng) = chooseInt 0 (length candidateMoves - 1) rng
                in case candidateMoves !!? moveIndex of
                    Nothing -> pure GameAborted
                    Just chosenMove ->
                        let actionMetrics = recordChosenMove gameState chosenMove nextTurnMetrics
                            (nextGameState, _) = Core.makeMove gameState chosenMove
                            resolvedMetrics = observeTransition gameState nextGameState actionMetrics
                        in stepGame shouldTrace turnLimit resolvedMetrics nextGameState nextRng))
  where
    turnCount = completedDecisionCount simulationMetrics
    playerMoves = Core.enumerateActivePlayerOptions gameState

bestMoves :: Core.GameState -> [Core.PlayerMove] -> [Core.PlayerMove]
bestMoves gameState playerMoves =
    case Core.heuristicHint 1 gameState playerMoves of
        [] -> playerMoves
        allHints@((firstScore, _) : rest) ->
            let bestScore = foldl' max firstScore (map fst rest)
                preferredMoves = map snd (filter ((== bestScore) . fst) allHints)
             in if null preferredMoves then playerMoves else preferredMoves

chooseInt :: Int -> Int -> StdGen -> (Int, StdGen)
chooseInt low high rng = uniformR (low, high) rng

isGameOver :: Core.GameState -> Bool
isGameOver = \case
    Core.CursedTreasureGame CursedTreasure.GameState { gameOver = isOver } -> isOver
    Core.FogOfBattleGame FogOfBattle.GameState { gameOver = isOver } -> isOver
    Core.ArtOfWarGame ArtOfWar.GameState { gameOver = isOver } -> isOver
    Core.RealEstateGame RealEstate.GameState { gameOver = isOver } -> isOver

accumulateResult :: DumbPlayStats -> GameResult -> DumbPlayStats
accumulateResult stats = \case
    GameCompleted completedGame ->
        let roundCount = completedGame.completedRoundCount
            simulationMetrics = completedGame.completedSimulationMetrics
         in accumulateSimulationMetrics simulationMetrics
                (stats
            { gamesCompleted = stats.gamesCompleted + 1
            , totalRounds = stats.totalRounds + roundCount
            , minRounds = Just $ maybe roundCount (min roundCount) stats.minRounds
            , maxRounds = Just $ maybe roundCount (max roundCount) stats.maxRounds
            , scoreDistribution = foldl' (flip bumpDistribution) stats.scoreDistribution completedGame.completedScores
            , winnerSpreadDistribution = bumpDistribution completedGame.completedWinnerSpread stats.winnerSpreadDistribution
            })
    GameAborted ->
        stats
            { gamesAborted = stats.gamesAborted + 1
            }

renderDumbPlayStats :: DumbPlayStats -> Text
renderDumbPlayStats stats = unlines $
    [ "Dumb-play statistics"
    , "  Games requested:           " <> show stats.gamesRequested
    , "  Games completed:           " <> show stats.gamesCompleted
    , "  Games aborted:             " <> show stats.gamesAborted
    , "  Total rounds:              " <> show stats.totalRounds
    , "  Average rounds:            " <> renderAverageRounds stats
    , "  Min rounds:                " <> maybe "n/a" show stats.minRounds
    , "  Max rounds:                " <> maybe "n/a" show stats.maxRounds
    , "  Turns with clue play:      " <> renderTurnRate stats.turnsWithClue stats.totalPlayerTurns
    , "  Turns with move:           " <> renderTurnRate stats.turnsWithMove stats.totalPlayerTurns
    , "  Turns with dig:            " <> renderTurnRate stats.turnsWithDig stats.totalPlayerTurns
    , "  Avg rounds/successful dig: " <> renderAverageDigGap stats
    , "  Main-game avg legal/choice:" <> renderAverageLegalMoves stats.mainPhaseMetrics
    , "  Main-game forced choices:  " <> renderSingleMoveRate stats.mainPhaseMetrics
    , "  Main-game branching:       " <> renderBranchingFactor stats.mainPhaseMetrics
    , "  RaiseTreasure avg legal:   " <> renderAverageLegalMoves stats.raiseTreasurePhaseMetrics
    , "  RaiseTreasure forced:      " <> renderSingleMoveRate stats.raiseTreasurePhaseMetrics
    , "  RaiseTreasure branching:   " <> renderBranchingFactor stats.raiseTreasurePhaseMetrics
    ]
    <> renderDistribution "Score distribution" stats.scoreDistribution
    <> renderDistribution "Winner spread distribution" stats.winnerSpreadDistribution
    <> renderNotes

renderAverageRounds :: DumbPlayStats -> Text
renderAverageRounds stats
    | stats.gamesCompleted == 0 = "n/a"
    | otherwise = renderFixed2 (fromIntegral stats.totalRounds / fromIntegral stats.gamesCompleted)

renderAverageLegalMoves :: PhaseMetrics -> Text
renderAverageLegalMoves phaseMetrics
    | phaseMetrics.decisionCount == 0 = "n/a"
    | otherwise = renderFixed2 (fromIntegral phaseMetrics.legalMoveCount / fromIntegral phaseMetrics.decisionCount)

renderSingleMoveRate :: PhaseMetrics -> Text
renderSingleMoveRate phaseMetrics
    | phaseMetrics.decisionCount == 0 = "n/a"
    | otherwise =
        let pct = (100 * fromIntegral phaseMetrics.forcedDecisionCount) / fromIntegral phaseMetrics.decisionCount :: Double
         in renderFixed2 pct <> "%"

renderBranchingFactor :: PhaseMetrics -> Text
renderBranchingFactor phaseMetrics
    | phaseMetrics.branchingDecisionCount == 0 = "n/a"
    | otherwise = renderFixed2 (fromIntegral phaseMetrics.branchingLegalMoveCount / fromIntegral phaseMetrics.branchingDecisionCount)

renderTurnRate :: Int -> Int -> Text
renderTurnRate rateNumerator rateDenominator
    | rateDenominator == 0 = "n/a"
    | otherwise = renderFixed2 (100 * fromIntegral rateNumerator / fromIntegral rateDenominator :: Double) <> "%"

renderAverageDigGap :: DumbPlayStats -> Text
renderAverageDigGap stats
    | stats.successfulDigTurns == 0 = "n/a"
    | otherwise = renderFixed2 (fromIntegral stats.totalRounds / fromIntegral stats.successfulDigTurns)

renderDistribution :: Text -> [(Int, Int)] -> [Text]
renderDistribution title distribution =
    case sortOn fst distribution of
        [] -> ["  " <> title <> ": n/a"]
        orderedDistribution ->
            ("  " <> title <> ":")
                : map renderEntry orderedDistribution
  where
    renderEntry (bucket, count) = "    " <> show bucket <> ": " <> show count

renderFixed2 :: Double -> Text
renderFixed2 value = toText (showFFloat (Just 2) value "")

emptyPhaseMetrics :: PhaseMetrics
emptyPhaseMetrics = PhaseMetrics
    { decisionCount = 0
    , legalMoveCount = 0
    , forcedDecisionCount = 0
    , branchingDecisionCount = 0
    , branchingLegalMoveCount = 0
    }

emptySimulationMetrics :: SimulationMetrics
emptySimulationMetrics = SimulationMetrics
    { currentTurn = Nothing
    , pendingSuccessfulDigTreasures = Nothing
    , lastSuccessfulDigTurn = Nothing
    , mainMetrics = emptyPhaseMetrics
    , raiseTreasureMetrics = emptyPhaseMetrics
    , completedTurnCount = 0
    , clueTurnCount = 0
    , moveTurnCount = 0
    , digTurnCount = 0
    , successfulDigTurnCount = 0
    , successfulDigGapCount = 0
    , turnsBetweenSuccessfulDigs = 0
    }

completedDecisionCount :: SimulationMetrics -> Int
completedDecisionCount simulationMetrics =
    simulationMetrics.mainMetrics.decisionCount + simulationMetrics.raiseTreasureMetrics.decisionCount

beginTrackedTurn :: Core.GameState -> SimulationMetrics -> SimulationMetrics
beginTrackedTurn gameState simulationMetrics =
    case simulationMetrics.currentTurn of
        Just turnTracker | turnTracker.trackedTurnNumber == currentTurnNumber gameState -> simulationMetrics
        _ -> simulationMetrics { currentTurn = Just (emptyTurnTracker (currentTurnNumber gameState)) }

emptyTurnTracker :: Int -> TurnTracker
emptyTurnTracker turnNumber = TurnTracker
    { trackedTurnNumber = turnNumber
    , trackedCommittedMainChoice = False
    , trackedClueTurn = False
    , trackedMoveTurn = False
    , trackedDigTurn = False
    , trackedSuccessfulDig = False
    , trackedPendingDigTreasures = Nothing
    }

recordDecisionMetrics :: Core.GameState -> [Core.PlayerMove] -> SimulationMetrics -> SimulationMetrics
recordDecisionMetrics gameState playerMoves simulationMetrics
    | shouldIgnoreRaiseTreasureDecision gameState playerMoves = simulationMetrics
    | isRaiseTreasurePhase gameState = simulationMetrics { raiseTreasureMetrics = addDecisionCount legalCount simulationMetrics.raiseTreasureMetrics }
    | shouldIgnoreMainPhaseDecision simulationMetrics = simulationMetrics
    | otherwise = simulationMetrics { mainMetrics = addDecisionCount legalCount simulationMetrics.mainMetrics }
  where
    legalCount = adjustedLegalMoveCount gameState playerMoves

shouldIgnoreRaiseTreasureDecision :: Core.GameState -> [Core.PlayerMove] -> Bool
shouldIgnoreRaiseTreasureDecision gameState playerMoves =
    isRaiseTreasureViewPassOnly gameState playerMoves

isRaiseTreasureViewPassOnly :: Core.GameState -> [Core.PlayerMove] -> Bool
isRaiseTreasureViewPassOnly gameState playerMoves =
    isRaiseTreasurePhase gameState && playerMoves == [Core.CursedTreasurePlayerMove CursedTreasure.RaisingTreasurePass]

shouldIgnoreMainPhaseDecision :: SimulationMetrics -> Bool
shouldIgnoreMainPhaseDecision simulationMetrics =
    case simulationMetrics.currentTurn of
        Just turnTracker -> turnTracker.trackedCommittedMainChoice
        Nothing -> False

addDecisionCount :: Int -> PhaseMetrics -> PhaseMetrics
addDecisionCount legalCount phaseMetrics =
    phaseMetrics
        { decisionCount = phaseMetrics.decisionCount + 1
        , legalMoveCount = phaseMetrics.legalMoveCount + legalCount
        , forcedDecisionCount = phaseMetrics.forcedDecisionCount + if legalCount == 1 then 1 else 0
        , branchingDecisionCount = phaseMetrics.branchingDecisionCount + if legalCount > 1 then 1 else 0
        , branchingLegalMoveCount = phaseMetrics.branchingLegalMoveCount + if legalCount > 1 then legalCount else 0
        }

adjustedLegalMoveCount :: Core.GameState -> [Core.PlayerMove] -> Int
adjustedLegalMoveCount gameState playerMoves
    | isRaiseTreasurePhase gameState = length playerMoves
    | otherwise = nonJeepMoveCount + usefulJeepDestinationCount gameState playerMoves
  where
    nonJeepMoveCount = length (filter (not . isMoveJeep) playerMoves)

usefulJeepDestinationCount :: Core.GameState -> [Core.PlayerMove] -> Int
usefulJeepDestinationCount gameState playerMoves
    | not (any isMoveJeep playerMoves) = 0
    | otherwise =
        case gameState of
            Core.CursedTreasureGame cursedState -> max 1 (countLowMarkerTreasures cursedState + min 2 (countBoardTokens isAmulet cursedState))
            _ -> min 2 (length (filter isMoveJeep playerMoves))
  where
    isAmulet CursedTreasure.Amulet = True
    isAmulet _ = False

countBoardTokens :: (CursedTreasure.TerrainToken -> Bool) -> CursedTreasure.GameState -> Int
countBoardTokens tokenFilter gameState = length (filter tokenFilter (allBoardTokens gameState))

countLowMarkerTreasures :: CursedTreasure.GameState -> Int
countLowMarkerTreasures gameState =
    length (filter isLowMarkerCount markerCounts)
  where
    markerCounts = map (\color -> countBoardTokens (isMarkerOf color) gameState) CursedTreasure.allClueColors
    isLowMarkerCount markerCount = markerCount >= 1 && markerCount <= 10
    isMarkerOf color (CursedTreasure.ClueToken tokenColor) = tokenColor == color
    isMarkerOf _ _ = False

allBoardTokens :: CursedTreasure.GameState -> [CursedTreasure.TerrainToken]
allBoardTokens gameState = concatMap tokensAtHex (Map.elems hexMap)
  where
    Core.CubeCoordinateTokens _ hexMap = gameState.terrainBoard
    tokensAtHex (CursedTreasure.TerrainHex _ _ tokens) = tokens

isMoveJeep :: Core.PlayerMove -> Bool
isMoveJeep = \case
    Core.CursedTreasurePlayerMove (CursedTreasure.MoveJeep _ _) -> True
    _ -> False

recordChosenMove :: Core.GameState -> Core.PlayerMove -> SimulationMetrics -> SimulationMetrics
recordChosenMove gameState playerMove simulationMetrics =
    simulationMetrics
        { pendingSuccessfulDigTreasures = nextPendingSuccessfulDigTreasures
        , currentTurn = updateTrackedTurn <$> simulationMetrics.currentTurn
        }
  where
    nextPendingSuccessfulDigTreasures =
        case simulationMetrics.pendingSuccessfulDigTreasures of
            Just currentPending -> Just currentPending
            Nothing
                | isDigMove playerMove && not (isRaiseTreasurePhase gameState) -> Just (totalFoundTreasures gameState)
                | otherwise -> Nothing

    updateTrackedTurn turnTracker =
        turnTracker
            { trackedCommittedMainChoice = turnTracker.trackedCommittedMainChoice || commitsMainChoice playerMove
            , trackedClueTurn = turnTracker.trackedClueTurn || isClueMove playerMove
            , trackedMoveTurn = turnTracker.trackedMoveTurn || isMoveTurn playerMove
            , trackedDigTurn = turnTracker.trackedDigTurn || isDigMove playerMove
            }

commitsMainChoice :: Core.PlayerMove -> Bool
commitsMainChoice = \case
    Core.CursedTreasurePlayerMove CursedTreasure.PassTurn -> True
    Core.CursedTreasurePlayerMove (CursedTreasure.PlayClue _ _) -> True
    Core.CursedTreasurePlayerMove (CursedTreasure.MoveJeep _ _) -> True
    Core.CursedTreasurePlayerMove CursedTreasure.ExchangeClueCards -> True
    Core.CursedTreasurePlayerMove (CursedTreasure.UseAmuletPlayClue _ _) -> True
    Core.CursedTreasurePlayerMove CursedTreasure.UseAmuletExchangeCards -> True
    Core.CursedTreasurePlayerMove (CursedTreasure.RaiseTreasure _) -> True
    _ -> False

isClueMove :: Core.PlayerMove -> Bool
isClueMove = \case
    Core.CursedTreasurePlayerMove (CursedTreasure.PlayClue _ _) -> True
    Core.CursedTreasurePlayerMove (CursedTreasure.UseAmuletPlayClue _ _) -> True
    _ -> False

isMoveTurn :: Core.PlayerMove -> Bool
isMoveTurn = \case
    Core.CursedTreasurePlayerMove (CursedTreasure.MoveJeep _ _) -> True
    _ -> False

isDigMove :: Core.PlayerMove -> Bool
isDigMove = \case
    Core.CursedTreasurePlayerMove (CursedTreasure.RaiseTreasure _) -> True
    _ -> False

observeTransition :: Core.GameState -> Core.GameState -> SimulationMetrics -> SimulationMetrics
observeTransition previousGameState nextGameState =
    finalizeObservedTurn (isGameOver nextGameState) . resolvePendingDig nextGameState
  where
    resolvePendingDig gameState simulationMetrics =
        simulationMetrics
            { pendingSuccessfulDigTreasures = nextPendingSuccessfulDigTreasures
            , successfulDigTurnCount = simulationMetrics.successfulDigTurnCount + successfulDigIncrement
            }
      where
        successfulDigIncrement = boolToCount (successfulDigResolved && totalFoundTreasures gameState > startingTreasureCount)
        nextPendingSuccessfulDigTreasures
            | successfulDigResolved = Nothing
            | otherwise = simulationMetrics.pendingSuccessfulDigTreasures
        successfulDigResolved = isJust simulationMetrics.pendingSuccessfulDigTreasures && not (isRaiseTreasurePhase gameState)
        startingTreasureCount = fromMaybe 0 simulationMetrics.pendingSuccessfulDigTreasures

    finalizeObservedTurn gameEnded simulationMetrics
        | gameEnded || currentTurnNumber previousGameState /= currentTurnNumber nextGameState = finalizeTrackedTurnInternal simulationMetrics
        | otherwise = simulationMetrics

finalizeTrackedTurn :: Bool -> SimulationMetrics -> SimulationMetrics
finalizeTrackedTurn gameEnded simulationMetrics
    | gameEnded = finalizeTrackedTurnInternal simulationMetrics
    | otherwise = simulationMetrics

finalizeTrackedTurnInternal :: SimulationMetrics -> SimulationMetrics
finalizeTrackedTurnInternal simulationMetrics =
    case simulationMetrics.currentTurn of
        Nothing -> simulationMetrics
        Just turnTracker ->
            simulationMetrics
                { currentTurn = Nothing
                , lastSuccessfulDigTurn = nextSuccessfulDigTurn
                , completedTurnCount = simulationMetrics.completedTurnCount + 1
                , clueTurnCount = simulationMetrics.clueTurnCount + boolToCount turnTracker.trackedClueTurn
                , moveTurnCount = simulationMetrics.moveTurnCount + boolToCount turnTracker.trackedMoveTurn
                , digTurnCount = simulationMetrics.digTurnCount + boolToCount turnTracker.trackedDigTurn
                , successfulDigGapCount = simulationMetrics.successfulDigGapCount + boolToCount hasPriorSuccessfulDig
                , turnsBetweenSuccessfulDigs = simulationMetrics.turnsBetweenSuccessfulDigs + successfulDigGap
                }
          where
            hasPriorSuccessfulDig = False
            successfulDigGap =
                0
            nextSuccessfulDigTurn = simulationMetrics.lastSuccessfulDigTurn

boolToCount :: Bool -> Int
boolToCount True = 1
boolToCount False = 0

currentTurnNumber :: Core.GameState -> Int
currentTurnNumber = \case
    Core.CursedTreasureGame gameState -> gameState.turn
    _ -> 0

isRaiseTreasurePhase :: Core.GameState -> Bool
isRaiseTreasurePhase = \case
    Core.CursedTreasureGame gameState -> isJust gameState.raisingTreasure
    _ -> False

totalFoundTreasures :: Core.GameState -> Int
totalFoundTreasures = \case
    Core.CursedTreasureGame gameState -> sum (map (length . (.foundTreasures)) gameState.players)
    _ -> 0

accumulateSimulationMetrics :: SimulationMetrics -> DumbPlayStats -> DumbPlayStats
accumulateSimulationMetrics simulationMetrics stats =
    stats
        { totalPlayerTurns = stats.totalPlayerTurns + simulationMetrics.completedTurnCount
        , turnsWithClue = stats.turnsWithClue + simulationMetrics.clueTurnCount
        , turnsWithMove = stats.turnsWithMove + simulationMetrics.moveTurnCount
        , turnsWithDig = stats.turnsWithDig + simulationMetrics.digTurnCount
        , successfulDigTurns = stats.successfulDigTurns + simulationMetrics.successfulDigTurnCount
        , successfulDigIntervals = stats.successfulDigIntervals + simulationMetrics.successfulDigGapCount
        , totalTurnsBetweenSuccessfulDigs = stats.totalTurnsBetweenSuccessfulDigs + simulationMetrics.turnsBetweenSuccessfulDigs
        , mainPhaseMetrics = accumulatePhaseMetrics stats.mainPhaseMetrics simulationMetrics.mainMetrics
        , raiseTreasurePhaseMetrics = accumulatePhaseMetrics stats.raiseTreasurePhaseMetrics simulationMetrics.raiseTreasureMetrics
        }

accumulatePhaseMetrics :: PhaseMetrics -> PhaseMetrics -> PhaseMetrics
accumulatePhaseMetrics statsMetrics gameMetrics =
    PhaseMetrics
        { decisionCount = statsMetrics.decisionCount + gameMetrics.decisionCount
        , legalMoveCount = statsMetrics.legalMoveCount + gameMetrics.legalMoveCount
        , forcedDecisionCount = statsMetrics.forcedDecisionCount + gameMetrics.forcedDecisionCount
        , branchingDecisionCount = statsMetrics.branchingDecisionCount + gameMetrics.branchingDecisionCount
        , branchingLegalMoveCount = statsMetrics.branchingLegalMoveCount + gameMetrics.branchingLegalMoveCount
        }

summarizeCompletedGame :: Core.GameState -> SimulationMetrics -> CompletedGameStats
summarizeCompletedGame gameState simulationMetrics =
    let scores = extractScores gameState
        playerTurnCount = max simulationMetrics.completedTurnCount (currentTurnNumber gameState)
     in CompletedGameStats
            { completedRoundCount = completedRounds playerTurnCount (playerCount gameState)
            , completedPlayerTurnCount = playerTurnCount
            , completedSimulationMetrics = simulationMetrics
            , completedScores = scores
            , completedWinnerSpread = winnerSpread scores
            }

completedRounds :: Int -> Int -> Int
completedRounds turns totalPlayers
    | totalPlayers <= 0 = 0
    | otherwise = (turns + totalPlayers - 1) `div` totalPlayers

playerCount :: Core.GameState -> Int
playerCount = \case
    Core.CursedTreasureGame gameState -> length gameState.players
    Core.FogOfBattleGame gameState -> length gameState.players
    Core.ArtOfWarGame gameState -> length gameState.players
    Core.RealEstateGame gameState -> length gameState.players

extractScores :: Core.GameState -> [Int]
extractScores = \case
    Core.CursedTreasureGame CursedTreasure.GameState { players = players } -> map playerScore players
    Core.FogOfBattleGame _ -> []
    Core.ArtOfWarGame _ -> []
    Core.RealEstateGame _ -> []

playerScore :: CursedTreasure.PlayerState -> Int
playerScore CursedTreasure.PlayerState { score = playerStateScore } = scoreValue playerStateScore

scoreValue :: CursedTreasure.Score -> Int
scoreValue = \case
    CursedTreasure.CurrentScore value -> value
    CursedTreasure.WinnerScore value -> value

winnerSpread :: [Int] -> Int
winnerSpread scores =
    case reverse (sort scores) of
        topScore : runnerUp : _ -> topScore - runnerUp
        [topScore] -> topScore
        [] -> 0

bumpDistribution :: Int -> [(Int, Int)] -> [(Int, Int)]
bumpDistribution bucket = \case
    [] -> [(bucket, 1)]
    (existingBucket, count) : rest
        | existingBucket == bucket -> (existingBucket, count + 1) : rest
        | otherwise -> (existingBucket, count) : bumpDistribution bucket rest

renderNotes :: [Text]
renderNotes =
    [ "  Notes:"
    , "    - Games aborted are reported, but excluded from all other statistics."
    , "    - Main-game legal-move counts ignore raw MoveJeep branching and instead use low-marker treasures plus up to 2 amulet destinations."
    , "    - Clue turns and move turns are mostly disjoint, but dig turns can overlap with either because RaiseTreasure can follow a move or clue play in the same turn."
    ]