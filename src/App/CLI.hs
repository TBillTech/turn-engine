module App.CLI
    ( Command (..)
    , ExampleMode (..)
    , RulesetName (..)
    , parseCommand
    , renderUsage
    )
where

import qualified Data.Text as Text

data RulesetName
    = CursedTreasure
    deriving (Show, Eq)

data Command
    = DumbPlay Int RulesetName
    | Example RulesetName ExampleMode
    | MoveExampleRequest RulesetName Text
    | Service
    | Version
    | TestExample FilePath FilePath
    | Help Text
    deriving (Show, Eq)

data ExampleMode
    = ExampleTranscript
    | ExampleLineCount
    | ExampleHeaders
    | ExampleSummary
    | ExampleRequest Int
    | ExampleResponse Int
    | ExampleLine Int
    deriving (Show, Eq)

parseCommand :: [String] -> Command
parseCommand args =
    case args of
        ["--service"] -> Service
        ["-s"] -> Service
        ["--version"] -> Version
        ["--test-example", requestFilePath, responseFilePath] -> TestExample requestFilePath responseFilePath
        ["--dumbplay", gameCount, rulesetName] -> parseDumbPlay gameCount rulesetName
        ["-d", gameCount, rulesetName] -> parseDumbPlay gameCount rulesetName
        ["--move_example_request", rulesetName, moveName] -> parseMoveExampleRequest rulesetName moveName
        ["--example", rulesetName] -> parseExample [rulesetName]
        ["-e", rulesetName] -> parseExample [rulesetName]
        "--example" : exampleArgs -> parseExample exampleArgs
        "-e" : exampleArgs -> parseExample exampleArgs
        [] -> Help renderUsage
        _ -> Help $ "Unrecognized arguments.\n\n" <> renderUsage

parseMoveExampleRequest :: String -> String -> Command
parseMoveExampleRequest rulesetText moveName =
    case parseRulesetName rulesetText of
        Nothing -> Help $ "Unsupported ruleset for move example mode: " <> toText rulesetText <> "\n\n" <> renderUsage
        Just rulesetName -> MoveExampleRequest rulesetName (toText moveName)

parseDumbPlay :: String -> String -> Command
parseDumbPlay gameCountText rulesetText =
    case readMaybe gameCountText of
        Nothing -> Help $ "Game count must be an integer.\n\n" <> renderUsage
        Just gameCount
            | gameCount <= 0 -> Help $ "Game count must be positive.\n\n" <> renderUsage
            | otherwise ->
                case parseRulesetName rulesetText of
                    Nothing -> Help $ "Unsupported ruleset for dumb-play: " <> toText rulesetText <> "\n\n" <> renderUsage
                    Just rulesetName -> DumbPlay gameCount rulesetName

parseExample :: [String] -> Command
parseExample args =
    case args of
        [rulesetText] -> parseExampleMode rulesetText ExampleTranscript
        [rulesetText, modeText] ->
            case parseExampleModeArg modeText of
                Left err -> Help $ err <> "\n\n" <> renderUsage
                Right mode -> parseExampleMode rulesetText mode
        [rulesetText, modeText, indexText] ->
            case parseIndexedExampleMode modeText indexText of
                Left err -> Help $ err <> "\n\n" <> renderUsage
                Right mode -> parseExampleMode rulesetText mode
        _ -> Help $ "Invalid example arguments.\n\n" <> renderUsage

parseExampleMode :: String -> ExampleMode -> Command
parseExampleMode rulesetText mode =
    case parseRulesetName rulesetText of
        Nothing -> Help $ "Unsupported ruleset for example mode: " <> toText rulesetText <> "\n\n" <> renderUsage
        Just rulesetName -> Example rulesetName mode

parseExampleModeArg :: String -> Either Text ExampleMode
parseExampleModeArg modeText =
    case Text.toLower (toText modeText) of
        "linecount" -> Right ExampleLineCount
        "headers" -> Right ExampleHeaders
        "summary" -> Right ExampleSummary
        "request" -> Left "Example request mode requires an index."
        "response" -> Left "Example response mode requires an index."
        "line" -> Left "Example line mode requires an index."
        _ -> Left ("Unsupported example mode: " <> toText modeText)

parseIndexedExampleMode :: String -> String -> Either Text ExampleMode
parseIndexedExampleMode modeText indexText = do
    index <- parsePositiveIndex indexText
    case Text.toLower (toText modeText) of
        "request" -> Right (ExampleRequest index)
        "response" -> Right (ExampleResponse index)
        "line" -> Right (ExampleLine index)
        "linecount" -> Left "Example lineCount mode does not take an index."
        "headers" -> Left "Example headers mode does not take an index."
        "summary" -> Left "Example summary mode does not take an index."
        _ -> Left ("Unsupported example mode: " <> toText modeText)

parsePositiveIndex :: String -> Either Text Int
parsePositiveIndex indexText =
    case readMaybe indexText of
        Nothing -> Left "Example index must be an integer."
        Just index
            | index <= 0 -> Left "Example index must be positive."
            | otherwise -> Right index

parseRulesetName :: String -> Maybe RulesetName
parseRulesetName rulesetText =
    case Text.toLower (toText rulesetText) of
        "cursedtreasure" -> Just CursedTreasure
        "cursed-treasure" -> Just CursedTreasure
        "cursed_treasure" -> Just CursedTreasure
        _ -> Nothing

renderUsage :: Text
renderUsage = unlines
    [ "Usage:"
    , "  turn-engine --dumbplay <n> CursedTreasure"
    , "  turn-engine -d <n> CursedTreasure"
    , "  turn-engine --example CursedTreasure"
    , "  turn-engine -e CursedTreasure"
    , "  turn-engine --example CursedTreasure lineCount"
    , "  turn-engine --example CursedTreasure headers"
    , "  turn-engine --example CursedTreasure summary"
    , "  turn-engine --example CursedTreasure request <n>"
    , "  turn-engine --example CursedTreasure response <n>"
    , "  turn-engine --example CursedTreasure line <n>"
    , "  turn-engine --move_example_request CursedTreasure <PlayerMove>"
    , "  turn-engine --test-example <request-file> <response-file>"
    , "  turn-engine --version"
    , "  turn-engine --service"
    , "  turn-engine -s"
    ]