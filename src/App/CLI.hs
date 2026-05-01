module App.CLI
    ( Command (..)
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
    | Service
    | Help Text
    deriving (Show, Eq)

parseCommand :: [String] -> Command
parseCommand args =
    case args of
        ["--service"] -> Service
        ["-s"] -> Service
        ["--dumbplay", gameCount, rulesetName] -> parseDumbPlay gameCount rulesetName
        ["-d", gameCount, rulesetName] -> parseDumbPlay gameCount rulesetName
        [] -> Help renderUsage
        _ -> Help $ "Unrecognized arguments.\n\n" <> renderUsage

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
    , "  turn-engine --service"
    , "  turn-engine -s"
    ]