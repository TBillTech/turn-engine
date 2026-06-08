module App.Protocol
    ( ServiceRequest (..)
    , ServiceResponse (..)
    )
where

import Data.Aeson
    ( FromJSON (parseJSON)
    , Object
    , ToJSON (toJSON, toEncoding)
    , Value
    , object
    , pairs
    , withObject
    , (.:)
    , (.=)
    )
import Data.Aeson.Types (Parser)

import qualified Game.Core.API as CoreAPI
import qualified Game.Core.Types as Core

data ServiceRequest
    = GetGameSetupPlayers
    | CreateNewGame [Core.PlayerDescription] Int
    | EnumerateActivePlayerOptions Core.GameState
    | MakeMove Core.GameState Core.PlayerMove
    | HeuristicHint Int Core.GameState [Core.PlayerMove]
    | Summary Core.GameState
    deriving (Show, Eq)

instance FromJSON ServiceRequest where
    parseJSON = withObject "ServiceRequest" $ \request -> do
        requestType <- request .: "request" :: Parser Text
        case requestType of
            "getGameSetupPlayers" -> pure GetGameSetupPlayers
            "createNewGame" -> do
                players <- request .: "players"
                randomSeed <- request .: "randomSeed"
                _ <- either (fail . toString) pure (CoreAPI.validateSetupPlayers players)
                pure (CreateNewGame players randomSeed)
            "enumerateActivePlayerOptions" -> EnumerateActivePlayerOptions
                <$> parseValidatedGameState request
            "makeMove" -> MakeMove
                <$> parseValidatedGameState request
                <*> request .: "playerMove"
            "heuristicHint" -> HeuristicHint
                <$> request .: "level"
                <*> parseValidatedGameState request
                <*> request .: "playerMoves"
            "summary" -> Summary
                <$> parseValidatedGameState request
            _ -> fail ("Unsupported service request type: " <> toString requestType)

parseValidatedGameState :: Object -> Parser Core.GameState
parseValidatedGameState request = do
    gameState <- request .: "gameState"
    either (fail . toString) pure (CoreAPI.validateGameState gameState)

instance ToJSON ServiceRequest where
    toJSON = \case
        GetGameSetupPlayers -> object
            [ "request" .= ("getGameSetupPlayers" :: Text)
            ]
        CreateNewGame players randomSeed -> object
            [ "request" .= ("createNewGame" :: Text)
            , "players" .= players
            , "randomSeed" .= randomSeed
            ]
        EnumerateActivePlayerOptions gameState -> object
            [ "request" .= ("enumerateActivePlayerOptions" :: Text)
            , "gameState" .= gameState
            ]
        MakeMove gameState playerMove -> object
            [ "request" .= ("makeMove" :: Text)
            , "gameState" .= gameState
            , "playerMove" .= playerMove
            ]
        HeuristicHint level gameState playerMoves -> object
            [ "request" .= ("heuristicHint" :: Text)
            , "level" .= level
            , "gameState" .= gameState
            , "playerMoves" .= playerMoves
            ]
        Summary gameState -> object
            [ "request" .= ("summary" :: Text)
            , "gameState" .= gameState
            ]

    toEncoding = \case
        GetGameSetupPlayers ->
            pairs ("request" .= ("getGameSetupPlayers" :: Text))
        CreateNewGame players randomSeed ->
            pairs
                (  "request" .= ("createNewGame" :: Text)
                <> "players" .= players
                <> "randomSeed" .= randomSeed
                )
        EnumerateActivePlayerOptions gameState ->
            pairs
                (  "request" .= ("enumerateActivePlayerOptions" :: Text)
                <> "gameState" .= gameState
                )
        MakeMove gameState playerMove ->
            pairs
                (  "request" .= ("makeMove" :: Text)
                <> "gameState" .= gameState
                <> "playerMove" .= playerMove
                )
        HeuristicHint level gameState playerMoves ->
            pairs
                (  "request" .= ("heuristicHint" :: Text)
                <> "level" .= level
                <> "gameState" .= gameState
                <> "playerMoves" .= playerMoves
                )
        Summary gameState ->
            pairs
                (  "request" .= ("summary" :: Text)
                <> "gameState" .= gameState
                )

data ServiceResponse
    = GameSetupPlayers [(Text, [Core.PlayerDescription])]
    | NewGameCreated (Either Text (Core.GameState, [Core.CensoredGameState]))
    | ActivePlayerOptions [Core.PlayerMove]
    | MoveApplied (Core.GameState, [Core.CensoredGameState])
    | HintGenerated [(Int, Core.PlayerMove)]
    | SummaryGenerated Text
    | ServiceError Text
    deriving (Show, Eq)

instance FromJSON ServiceResponse where
    parseJSON = withObject "ServiceResponse" $ \response -> do
        responseType <- response .: "response" :: Parser Text
        case responseType of
            "gameSetupPlayers" -> GameSetupPlayers <$> (response .: "rulesets" >>= traverse parseRuleset)
            "newGameCreated" -> do
                status <- response .: "status" :: Parser Text
                NewGameCreated <$> case status of
                    "ok" -> Right
                        <$> ((,)
                            <$> response .: "gameState"
                            <*> response .: "playerViews")
                    "error" -> Left <$> response .: "message"
                    _ -> fail ("Unsupported newGameCreated status: " <> toString status)
            "activePlayerOptions" -> ActivePlayerOptions <$> response .: "playerMoves"
            "moveApplied" -> MoveApplied
                <$> ((,)
                    <$> response .: "gameState"
                    <*> response .: "playerViews")
            "hintGenerated" -> HintGenerated <$> (response .: "hints" >>= traverse parseHint)
            "summaryGenerated" -> SummaryGenerated <$> response .: "summary"
            "serviceError" -> ServiceError <$> response .: "message"
            _ -> fail ("Unsupported service response type: " <> toString responseType)

instance ToJSON ServiceResponse where
    toJSON = \case
        GameSetupPlayers rulesets -> object
            [ "response" .= ("gameSetupPlayers" :: Text)
            , "rulesets" .= fmap toRuleset rulesets
            ]
        NewGameCreated result -> object
            (["response" .= ("newGameCreated" :: Text)] <> case result of
                Right (gameState, playerViews) ->
                    [ "status" .= ("ok" :: Text)
                    , "gameState" .= gameState
                    , "playerViews" .= playerViews
                    ]
                Left message ->
                    [ "status" .= ("error" :: Text)
                    , "message" .= message
                    ])
        ActivePlayerOptions playerMoves -> object
            [ "response" .= ("activePlayerOptions" :: Text)
            , "playerMoves" .= playerMoves
            ]
        MoveApplied (gameState, playerViews) -> object
            [ "response" .= ("moveApplied" :: Text)
            , "gameState" .= gameState
            , "playerViews" .= playerViews
            ]
        HintGenerated hints -> object
            [ "response" .= ("hintGenerated" :: Text)
            , "hints" .= fmap toHint hints
            ]
        SummaryGenerated summary -> object
            [ "response" .= ("summaryGenerated" :: Text)
            , "summary" .= summary
            ]
        ServiceError message -> object
            [ "response" .= ("serviceError" :: Text)
            , "message" .= message
            ]

    toEncoding = \case
        GameSetupPlayers rulesets ->
            pairs
                (  "response" .= ("gameSetupPlayers" :: Text)
                <> "rulesets" .= fmap toRuleset rulesets
                )
        NewGameCreated result ->
            pairs $ case result of
                Right (gameState, playerViews) ->
                    "response" .= ("newGameCreated" :: Text)
                    <> "status" .= ("ok" :: Text)
                    <> "gameState" .= gameState
                    <> "playerViews" .= playerViews
                Left message ->
                    "response" .= ("newGameCreated" :: Text)
                    <> "status" .= ("error" :: Text)
                    <> "message" .= message
        ActivePlayerOptions playerMoves ->
            pairs
                (  "response" .= ("activePlayerOptions" :: Text)
                <> "playerMoves" .= playerMoves
                )
        MoveApplied (gameState, playerViews) ->
            pairs
                (  "response" .= ("moveApplied" :: Text)
                <> "gameState" .= gameState
                <> "playerViews" .= playerViews
                )
        HintGenerated hints ->
            pairs
                (  "response" .= ("hintGenerated" :: Text)
                <> "hints" .= fmap toHint hints
                )
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

toRuleset :: (Text, [Core.PlayerDescription]) -> Value
toRuleset (name, playerTemplates) = object
    [ "name" .= name
    , "playerTemplates" .= playerTemplates
    ]

parseRuleset :: Value -> Parser (Text, [Core.PlayerDescription])
parseRuleset = withObject "RulesetPlayers" $ \ruleset ->
    (,)
        <$> ruleset .: "name"
        <*> ruleset .: "playerTemplates"

toHint :: (Int, Core.PlayerMove) -> Value
toHint (score, playerMove) = object
    [ "score" .= score
    , "playerMove" .= playerMove
    ]

parseHint :: Value -> Parser (Int, Core.PlayerMove)
parseHint = withObject "Hint" $ \hint ->
    (,)
        <$> hint .: "score"
        <*> hint .: "playerMove"