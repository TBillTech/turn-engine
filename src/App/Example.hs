module App.Example
    ( runExample
    )
where

import Data.Aeson (ToJSON (toEncoding, toJSON), encode, object, pairs, (.=))
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import System.Random (StdGen, mkStdGen, uniformR)

import App.CLI (ExampleMode (..), RulesetName (..))
import App.Protocol
import App.Service (handleRequest)
import qualified Game.Core.API as CoreAPI
import qualified Game.ArtOfWar.Types as ArtOfWar
import qualified Game.Core.Types as Core
import qualified Game.CursedTreasure.API as CursedTreasure
import qualified Game.CursedTreasure.Types as CursedTreasure
import qualified Game.FogOfBattle.Types as FogOfBattle
import qualified Game.RealEstate.Types as RealEstate

runExample :: RulesetName -> ExampleMode -> IO ()
runExample rulesetName exampleMode =
    case buildInitialRequest rulesetName of
        Left err -> putTextLn err
        Right createGameRequest -> do
            let firstResponse = handleRequest GetGameSetupPlayers
                firstExchange = ExampleExchange GetGameSetupPlayers firstResponse
                createGameResponse = handleRequest createGameRequest
                secondExchange = ExampleExchange createGameRequest createGameResponse
            case createGameResponse of
                NewGameCreated (Right (gameState, _)) -> do
                    let remainingExchanges = playGame gameState (mkStdGen 0)
                    renderExampleOutput exampleMode (firstExchange : secondExchange : remainingExchanges)
                NewGameCreated (Left err) -> putTextLn err
                _ -> putTextLn "Unexpected response while creating example game."

data ExampleExchange = ExampleExchange
    { request :: ServiceRequest
    , response :: ServiceResponse
    }

buildInitialRequest :: RulesetName -> Either Text ServiceRequest
buildInitialRequest CursedTreasure =
    let selectedPlayerCount = 3
        setupPlayers = take selectedPlayerCount CursedTreasure.getGameSetupPlayers
     in Right (CreateNewGame (map Core.CursedTreasurePlayerDescription setupPlayers) 12345)

playGame :: Core.GameState -> StdGen -> [ExampleExchange]
playGame gameState rng
    | isGameOver gameState = []
    | otherwise =
        let optionRequest = EnumerateActivePlayerOptions gameState
            optionResponse = handleRequest optionRequest
            optionExchange = ExampleExchange optionRequest optionResponse
         in case optionResponse of
            ActivePlayerOptions playerMoves
                | null playerMoves -> [optionExchange]
                | otherwise ->
                    let candidateMoves = chooseCandidateMoves playerMoves hintResponse
                        (moveIndex, nextRng) = chooseInt 0 (length candidateMoves - 1) rng
                        hintRequest = HeuristicHint 1 gameState playerMoves
                        hintResponse = handleRequest hintRequest
                        hintExchange = ExampleExchange hintRequest hintResponse
                     in case candidateMoves !!? moveIndex of
                            Nothing -> [optionExchange, hintExchange]
                            Just chosenMove ->
                                let moveRequest = MakeMove gameState chosenMove
                                    moveResponse = handleRequest moveRequest
                                    moveExchange = ExampleExchange moveRequest moveResponse
                                 in case moveResponse of
                                        MoveApplied (nextGameState, _) ->
                                            optionExchange : hintExchange : moveExchange : playGame nextGameState nextRng
                                        _ -> [optionExchange, hintExchange, moveExchange]
            _ -> [optionExchange]

putEncoded :: ToJSON a => a -> IO ()
putEncoded = LazyByteString.putStrLn . encode

renderExampleOutput :: ExampleMode -> [ExampleExchange] -> IO ()
renderExampleOutput exampleMode exchanges =
    case exampleMode of
        ExampleTranscript -> traverse_ putFullExchange exchanges
        ExampleLineCount -> putTextLn (show (length exchanges * 2))
        ExampleHeaders -> traverse_ putHeaderExchange exchanges
        ExampleSummary -> traverse_ putSummaryExchange exchanges
        ExampleRequest index -> renderIndexedRequest index (map (.request) exchanges)
        ExampleResponse index -> renderIndexedResponse index (map (.response) exchanges)
        ExampleLine index -> renderIndexedLine index (flattenTranscript exchanges)

putFullExchange :: ExampleExchange -> IO ()
putFullExchange exchange = do
    putEncoded exchange.request
    putEncoded exchange.response

putHeaderExchange :: ExampleExchange -> IO ()
putHeaderExchange exchange = do
    putEncoded (HeaderRequest exchange.request)
    putEncoded (HeaderResponse exchange.response)

putSummaryExchange :: ExampleExchange -> IO ()
putSummaryExchange exchange = do
    putEncoded (SummaryRequest exchange.request)
    putEncoded (SummaryResponse exchange.response)

renderIndexedRequest :: Int -> [ServiceRequest] -> IO ()
renderIndexedRequest index requests =
    case requests !!? (index - 1) of
        Nothing -> putTextLn ("No example request at index " <> show index <> ".")
        Just request -> putEncoded request

renderIndexedResponse :: Int -> [ServiceResponse] -> IO ()
renderIndexedResponse index responses =
    case responses !!? (index - 1) of
        Nothing -> putTextLn ("No example response at index " <> show index <> ".")
        Just response -> putEncoded response

renderIndexedLine :: Int -> [ExampleTranscriptLine] -> IO ()
renderIndexedLine index transcriptLines =
    case transcriptLines !!? (index - 1) of
        Nothing -> putTextLn ("No example line at index " <> show index <> ".")
        Just transcriptLine -> putTranscriptLine transcriptLine

data ExampleTranscriptLine
    = TranscriptRequest ServiceRequest
    | TranscriptResponse ServiceResponse

flattenTranscript :: [ExampleExchange] -> [ExampleTranscriptLine]
flattenTranscript = concatMap $ \exchange -> [TranscriptRequest exchange.request, TranscriptResponse exchange.response]

putTranscriptLine :: ExampleTranscriptLine -> IO ()
putTranscriptLine = \case
    TranscriptRequest request -> putEncoded request
    TranscriptResponse response -> putEncoded response

newtype HeaderRequest = HeaderRequest ServiceRequest

instance ToJSON HeaderRequest where
    toJSON (HeaderRequest request) = case request of
        GetGameSetupPlayers -> object
            [ "request" .= ("getGameSetupPlayers" :: Text)
            ]
        CreateNewGame _ randomSeed -> object
            [ "request" .= ("createNewGame" :: Text)
            , "randomSeed" .= randomSeed
            ]
        EnumerateActivePlayerOptions _ -> object
            [ "request" .= ("enumerateActivePlayerOptions" :: Text)
            ]
        MakeMove _ playerMove -> object
            [ "request" .= ("makeMove" :: Text)
            , "playerMove" .= playerMove
            ]
        HeuristicHint level _ _ -> object
            [ "request" .= ("heuristicHint" :: Text)
            , "level" .= level
            ]
        Summary _ -> object
            [ "request" .= ("summary" :: Text)
            ]

    toEncoding (HeaderRequest request) = case request of
        GetGameSetupPlayers ->
            pairs ("request" .= ("getGameSetupPlayers" :: Text))
        CreateNewGame _ randomSeed ->
            pairs
                (  "request" .= ("createNewGame" :: Text)
                <> "randomSeed" .= randomSeed
                )
        EnumerateActivePlayerOptions _ ->
            pairs ("request" .= ("enumerateActivePlayerOptions" :: Text))
        MakeMove _ playerMove ->
            pairs
                (  "request" .= ("makeMove" :: Text)
                <> "playerMove" .= playerMove
                )
        HeuristicHint level _ _ ->
            pairs
                (  "request" .= ("heuristicHint" :: Text)
                <> "level" .= level
                )
        Summary _ ->
            pairs ("request" .= ("summary" :: Text))

newtype HeaderResponse = HeaderResponse ServiceResponse

instance ToJSON HeaderResponse where
    toJSON (HeaderResponse response) = case response of
        GameSetupPlayers _ -> object
            [ "response" .= ("gameSetupPlayers" :: Text)
            ]
        NewGameCreated result -> object
            (["response" .= ("newGameCreated" :: Text)] <> case result of
                Right _ -> ["status" .= ("ok" :: Text)]
                Left message ->
                    [ "status" .= ("error" :: Text)
                    , "message" .= message
                    ])
        ActivePlayerOptions _ -> object
            [ "response" .= ("activePlayerOptions" :: Text)
            ]
        MoveApplied _ -> object
            [ "response" .= ("moveApplied" :: Text)
            ]
        HintGenerated _ -> object
            [ "response" .= ("hintGenerated" :: Text)
            ]
        SummaryGenerated _ -> object
            [ "response" .= ("summaryGenerated" :: Text)
            ]
        ServiceError message -> object
            [ "response" .= ("serviceError" :: Text)
            , "message" .= message
            ]

    toEncoding (HeaderResponse response) = case response of
        GameSetupPlayers _ ->
            pairs ("response" .= ("gameSetupPlayers" :: Text))
        NewGameCreated result ->
            pairs $ case result of
                Right _ ->
                    "response" .= ("newGameCreated" :: Text)
                    <> "status" .= ("ok" :: Text)
                Left message ->
                    "response" .= ("newGameCreated" :: Text)
                    <> "status" .= ("error" :: Text)
                    <> "message" .= message
        ActivePlayerOptions _ ->
            pairs ("response" .= ("activePlayerOptions" :: Text))
        MoveApplied _ ->
            pairs ("response" .= ("moveApplied" :: Text))
        HintGenerated _ ->
            pairs ("response" .= ("hintGenerated" :: Text))
        SummaryGenerated _ ->
            pairs ("response" .= ("summaryGenerated" :: Text))
        ServiceError message ->
            pairs
                (  "response" .= ("serviceError" :: Text)
                <> "message" .= message
                )

newtype SummaryRequest = SummaryRequest ServiceRequest

instance ToJSON SummaryRequest where
    toJSON (SummaryRequest request) = case request of
        GetGameSetupPlayers -> object
            [ "request" .= ("getGameSetupPlayers" :: Text)
            ]
        CreateNewGame _ randomSeed -> object
            [ "request" .= ("createNewGame" :: Text)
            , "randomSeed" .= randomSeed
            ]
        EnumerateActivePlayerOptions gameState -> object
            [ "request" .= ("enumerateActivePlayerOptions" :: Text)
            , "summary" .= CoreAPI.summary gameState
            ]
        MakeMove gameState playerMove -> object
            [ "request" .= ("makeMove" :: Text)
            , "playerMove" .= playerMove
            , "summary" .= CoreAPI.summary gameState
            ]
        HeuristicHint level gameState _ -> object
            [ "request" .= ("heuristicHint" :: Text)
            , "level" .= level
            , "summary" .= CoreAPI.summary gameState
            ]
        Summary gameState -> object
            [ "request" .= ("summary" :: Text)
            , "summary" .= CoreAPI.summary gameState
            ]

    toEncoding (SummaryRequest request) = case request of
        GetGameSetupPlayers ->
            pairs ("request" .= ("getGameSetupPlayers" :: Text))
        CreateNewGame _ randomSeed ->
            pairs
                (  "request" .= ("createNewGame" :: Text)
                <> "randomSeed" .= randomSeed
                )
        EnumerateActivePlayerOptions gameState ->
            pairs
                (  "request" .= ("enumerateActivePlayerOptions" :: Text)
                <> "summary" .= CoreAPI.summary gameState
                )
        MakeMove gameState playerMove ->
            pairs
                (  "request" .= ("makeMove" :: Text)
                <> "playerMove" .= playerMove
                <> "summary" .= CoreAPI.summary gameState
                )
        HeuristicHint level gameState _ ->
            pairs
                (  "request" .= ("heuristicHint" :: Text)
                <> "level" .= level
                <> "summary" .= CoreAPI.summary gameState
                )
        Summary gameState ->
            pairs
                (  "request" .= ("summary" :: Text)
                <> "summary" .= CoreAPI.summary gameState
                )

newtype SummaryResponse = SummaryResponse ServiceResponse

instance ToJSON SummaryResponse where
    toJSON (SummaryResponse response) = case response of
        GameSetupPlayers _ -> object
            [ "response" .= ("gameSetupPlayers" :: Text)
            ]
        NewGameCreated result -> object
            (["response" .= ("newGameCreated" :: Text)] <> case result of
                Right (gameState, _) ->
                    [ "status" .= ("ok" :: Text)
                    , "summary" .= CoreAPI.summary gameState
                    ]
                Left message ->
                    [ "status" .= ("error" :: Text)
                    , "message" .= message
                    ])
        ActivePlayerOptions _ -> object
            [ "response" .= ("activePlayerOptions" :: Text)
            ]
        MoveApplied (gameState, _) -> object
            [ "response" .= ("moveApplied" :: Text)
            , "summary" .= CoreAPI.summary gameState
            ]
        HintGenerated _ -> object
            [ "response" .= ("hintGenerated" :: Text)
            ]
        SummaryGenerated summary -> object
            [ "response" .= ("summaryGenerated" :: Text)
            , "summary" .= summary
            ]
        ServiceError message -> object
            [ "response" .= ("serviceError" :: Text)
            , "message" .= message
            ]

    toEncoding (SummaryResponse response) = case response of
        GameSetupPlayers _ ->
            pairs ("response" .= ("gameSetupPlayers" :: Text))
        NewGameCreated result ->
            pairs $ case result of
                Right (gameState, _) ->
                    "response" .= ("newGameCreated" :: Text)
                    <> "status" .= ("ok" :: Text)
                    <> "summary" .= CoreAPI.summary gameState
                Left message ->
                    "response" .= ("newGameCreated" :: Text)
                    <> "status" .= ("error" :: Text)
                    <> "message" .= message
        ActivePlayerOptions _ ->
            pairs ("response" .= ("activePlayerOptions" :: Text))
        MoveApplied (gameState, _) ->
            pairs
                (  "response" .= ("moveApplied" :: Text)
                <> "summary" .= CoreAPI.summary gameState
                )
        HintGenerated _ ->
            pairs ("response" .= ("hintGenerated" :: Text))
        SummaryGenerated summary ->
            pairs
                (  "response" .= ("summaryGenerated" :: Text)
                <> "summary" .= summary
                )
        ServiceError message ->
            pairs
                (  "response" .= ("serviceError" :: Text)
                <> "message" .= message
                )

chooseCandidateMoves :: [Core.PlayerMove] -> ServiceResponse -> [Core.PlayerMove]
chooseCandidateMoves playerMoves = \case
    HintGenerated [] -> playerMoves
    HintGenerated allHints@((firstScore, _) : rest) ->
        let bestScore = foldl' max firstScore (map fst rest)
            preferredMoves = map snd (filter ((== bestScore) . fst) allHints)
         in if null preferredMoves then playerMoves else preferredMoves
    _ -> playerMoves

chooseInt :: Int -> Int -> StdGen -> (Int, StdGen)
chooseInt low high rng = uniformR (low, high) rng

isGameOver :: Core.GameState -> Bool
isGameOver = \case
    Core.CursedTreasureGame CursedTreasure.GameState { gameOver = gameOver } -> gameOver
    Core.FogOfBattleGame FogOfBattle.GameState { gameOver = gameOver } -> gameOver
    Core.ArtOfWarGame ArtOfWar.GameState { gameOver = gameOver } -> gameOver
    Core.RealEstateGame RealEstate.GameState { gameOver = gameOver } -> gameOver