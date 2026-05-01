module App.DumbPlay
    ( DumbPlayStats (..)
    , runDumbPlay
    , renderDumbPlayStats
    )
where

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
    , totalTurns :: Int
    , minTurns :: Maybe Int
    , maxTurns :: Maybe Int
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
        , totalTurns = 0
        , minTurns = Nothing
        , maxTurns = Nothing
        }

data GameResult = GameCompleted Int | GameAborted Int

simulateGame :: Bool -> RulesetName -> StdGen -> IO GameResult
simulateGame shouldTrace rulesetName rng0 =
    case createInitialGame rulesetName rng0 of
        Left _ -> pure (GameAborted 0)
        Right (gameState, rng1) -> stepGame shouldTrace 0 10000 gameState rng1

createInitialGame :: RulesetName -> StdGen -> Either Text (Core.GameState, StdGen)
createInitialGame CursedTreasure rng0 =
    let (playerCount, rng1) = chooseInt 2 4 rng0
        setupPlayers = take playerCount CursedTreasure.getGameSetupPlayers
        seedValue = 12345
     in case Core.createNewGame (map Core.CursedTreasurePlayerDescription setupPlayers) seedValue of
            Left err -> Left err
            Right (gameState, _) -> Right (gameState, rng1)

stepGame :: Bool -> Int -> Int -> Core.GameState -> StdGen -> IO GameResult
stepGame shouldTrace turnCount turnLimit gameState rng = do
    when shouldTrace $ do
        putTextLn (Core.summary gameState)
        hFlush stdout
    if isGameOver gameState then
        pure (GameCompleted turnCount)
    else    (if (turnCount >= turnLimit) 
            || null playerMoves then 
                pure (GameAborted turnCount) else 
                (let    candidateMoves = bestMoves gameState playerMoves
                        (moveIndex, nextRng) = chooseInt 0 (length candidateMoves - 1) rng
                in case candidateMoves !!? moveIndex of
                    Nothing -> pure (GameAborted turnCount)
                    Just chosenMove ->
                        let (nextGameState, _) = Core.makeMove gameState chosenMove
                        in stepGame shouldTrace (turnCount + 1) turnLimit nextGameState nextRng))
  where
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
    GameCompleted turnCount ->
        stats
            { gamesCompleted = stats.gamesCompleted + 1
            , totalTurns = stats.totalTurns + turnCount
            , minTurns = Just $ maybe turnCount (min turnCount) stats.minTurns
            , maxTurns = Just $ maybe turnCount (max turnCount) stats.maxTurns
            }
    GameAborted _ ->
        stats
            { gamesAborted = stats.gamesAborted + 1
            }

renderDumbPlayStats :: DumbPlayStats -> Text
renderDumbPlayStats stats = unlines
    [ "Dumb-play statistics"
    , "  Games requested: " <> show stats.gamesRequested
    , "  Games completed: " <> show stats.gamesCompleted
    , "  Games aborted:   " <> show stats.gamesAborted
    , "  Total turns:     " <> show stats.totalTurns
    , "  Average turns:   " <> renderAverage stats
    , "  Min turns:       " <> maybe "n/a" show stats.minTurns
    , "  Max turns:       " <> maybe "n/a" show stats.maxTurns
    ]

renderAverage :: DumbPlayStats -> Text
renderAverage stats
    | stats.gamesCompleted == 0 = "n/a"
    | otherwise = show (stats.totalTurns `div` stats.gamesCompleted)