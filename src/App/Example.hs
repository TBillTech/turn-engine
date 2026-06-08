module App.Example
    ( runExample
    , runMoveExampleRequest
    , buildMoveExampleRequest
    )
where

import Data.Aeson (ToJSON (toEncoding, toJSON), encode, object, pairs, (.=))
import qualified Data.Map.Strict as Map
import System.Random (StdGen, mkStdGen, uniformR)

import App.CLI (ExampleMode (..), RulesetName (..))
import App.Protocol
import App.Service (handleRequest)
import qualified Game.Core.API as CoreAPI
import qualified Game.ArtOfWar.Types as ArtOfWar
import qualified Game.Core.Types as Core
import Game.Core.Primitives (CubeCoordinateTokens (CubeCoordinateTokens), CubeCoordinate, ToHourHand (toHourHand), mkCubeCoordinate)
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

runMoveExampleRequest :: RulesetName -> Text -> IO ()
runMoveExampleRequest rulesetName moveName =
    case buildMoveExampleRequest rulesetName moveName of
        Left err -> putTextLn err
        Right request -> putEncoded request

buildMoveExampleRequest :: RulesetName -> Text -> Either Text ServiceRequest
buildMoveExampleRequest CursedTreasure moveName = do
    move <- buildCursedTreasureMoveExample moveName
    pure $ MakeMove (Core.CursedTreasureGame move.gameState) (Core.CursedTreasurePlayerMove move.playerMove)

data CursedTreasureMoveExample = CursedTreasureMoveExample
    { gameState :: CursedTreasure.GameState
    , playerMove :: CursedTreasure.PlayerMove
    }

buildCursedTreasureMoveExample :: Text -> Either Text CursedTreasureMoveExample
buildCursedTreasureMoveExample moveName
    | moveName == "PlayerMoveError" =
        Right (CursedTreasureMoveExample nominalGameState (CursedTreasure.PlayerMoveError "example move error"))
    | moveName `elem` raisingTreasureMoveNames =
        buildEnumeratedMoveExample moveName (if moveName `elem` ["RaisingTreasurePass", "RaisingTreasureTake"] then raisingTreasureGameState else raisingTreasureCurseGameState)
    | otherwise =
        buildEnumeratedMoveExample moveName (adjustNominalGameState moveName nominalGameState)

buildEnumeratedMoveExample :: Text -> CursedTreasure.GameState -> Either Text CursedTreasureMoveExample
buildEnumeratedMoveExample moveName gameState =
    case find (matchesMoveConstructor moveName) (CursedTreasure.enumerateActivePlayerOptions gameState) of
        Just move -> Right (CursedTreasureMoveExample gameState move)
        Nothing -> Left ("No legal CursedTreasure move example found for constructor " <> moveName <> ".")

matchesMoveConstructor :: Text -> CursedTreasure.PlayerMove -> Bool
matchesMoveConstructor moveName playerMove =
    case moveName of
        "PlayerMoveError" -> case playerMove of CursedTreasure.PlayerMoveError _ -> True; _ -> False
        "PassTurn" -> playerMove == CursedTreasure.PassTurn
        "PlayClue" -> case playerMove of CursedTreasure.PlayClue _ _ -> True; _ -> False
        "MoveJeep" -> case playerMove of CursedTreasure.MoveJeep _ _ -> True; _ -> False
        "ExchangeClueCards" -> playerMove == CursedTreasure.ExchangeClueCards
        "PickupAmulet" -> playerMove == CursedTreasure.PickupAmulet
        "UseAmuletIncrMove" -> playerMove == CursedTreasure.UseAmuletIncrMove
        "UseAmuletPlayClue" -> case playerMove of CursedTreasure.UseAmuletPlayClue _ _ -> True; _ -> False
        "UseAmuletExchangeCards" -> playerMove == CursedTreasure.UseAmuletExchangeCards
        "UseAmuletRemoveSiteMarker" -> case playerMove of CursedTreasure.UseAmuletRemoveSiteMarker _ _ _ -> True; _ -> False
        "RaiseTreasure" -> case playerMove of CursedTreasure.RaiseTreasure _ -> True; _ -> False
        "RaisingTreasurePass" -> playerMove == CursedTreasure.RaisingTreasurePass
        "RaisingTreasureTake" -> playerMove == CursedTreasure.RaisingTreasureTake
        "RaisingTreasureWardCurse" -> playerMove == CursedTreasure.RaisingTreasureWardCurse
        "RaisingTreasureAcceptCurse" -> playerMove == CursedTreasure.RaisingTreasureAcceptCurse
        _ -> False

raisingTreasureMoveNames :: [Text]
raisingTreasureMoveNames =
    [ "RaisingTreasurePass"
    , "RaisingTreasureTake"
    , "RaisingTreasureWardCurse"
    , "RaisingTreasureAcceptCurse"
    ]

nominalGameState :: CursedTreasure.GameState
nominalGameState =
    baseCursedTreasureGameState
        { CursedTreasure.players = [activePlayerState, secondPlayerState]
        , CursedTreasure.terrainBoard = boardFromList nominalBoardHexes
        , CursedTreasure.activePlayer = activePlayerId
        , CursedTreasure.playerTurn = activePlayerId
        }
  where
    activePlayerState =
        baseActivePlayerState
            { CursedTreasure.amulets = 1
            , CursedTreasure.clues = [jungleClue]
            , CursedTreasure.availableJeepMoves = 1
            , CursedTreasure.availableCluePlays = 1
            , CursedTreasure.availablePickupAmulet = 1
            , CursedTreasure.availableClueCardExchange = 1
            }
    secondPlayerState = baseSecondPlayerState
    nominalBoardHexes =
        [ (origin, CursedTreasure.TerrainHex False CursedTreasure.Meadow [CursedTreasure.PlayerJeep activePlayerId, CursedTreasure.Amulet, CursedTreasure.ClueToken firstClueColor])
        , (adjacentOrigin, CursedTreasure.TerrainHex False CursedTreasure.Jungle [CursedTreasure.ClueToken firstClueColor])
        , (beachOrigin, CursedTreasure.TerrainHex False CursedTreasure.Beach [])
        , (oceanOrigin, CursedTreasure.TerrainHex True CursedTreasure.Ocean [])
        ]

raisingTreasureGameState :: CursedTreasure.GameState
raisingTreasureGameState =
    nominalGameState
        { CursedTreasure.players = [activePlayerState, secondPlayerState]
        , CursedTreasure.raisingTreasure = Just treasureState
        }
  where
    treasureState =
        CursedTreasure.RaisingTreasureState
            { CursedTreasure.rtTreasureChest = ([CursedTreasure.Treasure 4], [])
            , CursedTreasure.rtOrder = [activePlayerId]
            , CursedTreasure.rtPlayerIndex = 0
            , CursedTreasure.rtViewing = []
            }
    activePlayerState = baseActivePlayerState
    secondPlayerState = baseSecondPlayerState

raisingTreasureCurseGameState :: CursedTreasure.GameState
raisingTreasureCurseGameState =
    nominalGameState
        { CursedTreasure.players = [activePlayerState, secondPlayerState]
        , CursedTreasure.raisingTreasure = Just treasureState
        }
  where
    treasureState =
        CursedTreasure.RaisingTreasureState
            { CursedTreasure.rtTreasureChest = ([CursedTreasure.Curse, CursedTreasure.Treasure 4], [])
            , CursedTreasure.rtOrder = [activePlayerId]
            , CursedTreasure.rtPlayerIndex = 0
            , CursedTreasure.rtViewing = []
            }
    activePlayerState = baseActivePlayerState { CursedTreasure.amulets = 1, CursedTreasure.foundTreasures = [CursedTreasure.Treasure 8, CursedTreasure.Treasure 5] }
    secondPlayerState = baseSecondPlayerState

adjustNominalGameState :: Text -> CursedTreasure.GameState -> CursedTreasure.GameState
adjustNominalGameState moveName gameState =
    gameState
        { CursedTreasure.players = map adjustPlayer gameState.players
        , CursedTreasure.terrainBoard = adjustBoard moveName gameState.terrainBoard
        }
  where
    adjustPlayer playerState
        | playerState.player.playerId /= activePlayerId = playerState
        | otherwise =
            case moveName of
                "UseAmuletIncrMove" -> playerState { CursedTreasure.availableJeepMoves = 0 }
                "UseAmuletPlayClue" -> playerState { CursedTreasure.availableCluePlays = 0 }
                "UseAmuletExchangeCards" -> playerState { CursedTreasure.availableClueCardExchange = 0 }
                "UseAmuletRemoveSiteMarker" -> playerState { CursedTreasure.availableCluePlays = 0, CursedTreasure.availableClueCardExchange = 0 }
                _ -> playerState

    adjustBoard "RaiseTreasure" _ =
        boardFromList
            [ (origin, CursedTreasure.TerrainHex False CursedTreasure.Meadow [CursedTreasure.PlayerJeep activePlayerId, CursedTreasure.Amulet, CursedTreasure.ClueToken firstClueColor])
            , (adjacentOrigin, CursedTreasure.TerrainHex False CursedTreasure.Jungle [])
            , (beachOrigin, CursedTreasure.TerrainHex False CursedTreasure.Beach [])
            , (oceanOrigin, CursedTreasure.TerrainHex True CursedTreasure.Ocean [])
            ]
    adjustBoard _ board = board

baseCursedTreasureGameState :: CursedTreasure.GameState
baseCursedTreasureGameState = fst (CursedTreasure.createNewGame (take 2 CursedTreasure.getGameSetupPlayers) 12345)

baseActivePlayerState :: CursedTreasure.PlayerState
baseActivePlayerState =
    case baseCursedTreasureGameState.players of
        playerState : _ -> playerState
        [] -> error "Expected at least one CursedTreasure player"

baseSecondPlayerState :: CursedTreasure.PlayerState
baseSecondPlayerState =
    case drop 1 baseCursedTreasureGameState.players of
        playerState : _ -> playerState
        [] -> error "Expected at least two CursedTreasure players"

activePlayerId :: CursedTreasure.PlayerId
activePlayerId = baseActivePlayerState.player.playerId

firstClueColor :: CursedTreasure.ClueColor
firstClueColor =
    case CursedTreasure.allClueColors of
        clueColor : _ -> clueColor
        [] -> error "Expected at least one clue color"

jungleClue :: CursedTreasure.ClueCard
jungleClue = CursedTreasure.IsOn (CursedTreasure.FeatureClue False CursedTreasure.Jungle)

boardFromList :: [(CubeCoordinate Int, CursedTreasure.TerrainHex)] -> CursedTreasure.HexBoard
boardFromList = CubeCoordinateTokens (toHourHand (0 :: Int)) . Map.fromList

origin :: CubeCoordinate Int
origin = mkCubeCoordinate 0 0

adjacentOrigin :: CubeCoordinate Int
adjacentOrigin = mkCubeCoordinate 1 0

beachOrigin :: CubeCoordinate Int
beachOrigin = mkCubeCoordinate 0 1

oceanOrigin :: CubeCoordinate Int
oceanOrigin = mkCubeCoordinate (-1) 0

data ExampleExchange = ExampleExchange
    { request :: ServiceRequest
    , response :: ServiceResponse
    }

buildInitialRequest :: RulesetName -> Either Text ServiceRequest
buildInitialRequest CursedTreasure =
    let selectedPlayerCount = 3
        setupPlayers = zipWith renamePlayer cursedTreasureExampleNames (take selectedPlayerCount CursedTreasure.getGameSetupPlayers)
    in Right (CreateNewGame setupPlayers 12345)

cursedTreasureExampleNames :: [Text]
cursedTreasureExampleNames =
    [ "Jose Álvarez"
    , "Miyu 星"
    , "Zoe Faßbinder"
    ]

renamePlayer :: Text -> CursedTreasure.PlayerDescription -> CursedTreasure.PlayerDescription
renamePlayer playerName player =
    player{CursedTreasure.playerName = playerName}

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
putEncoded = putLBSLn . encode

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