module Main where

import App.CLI (Command (..), parseCommand)
import App.DumbPlay (renderDumbPlayStats, runDumbPlay)
import App.Example (runExample)
import App.Service (runService)

main :: IO ()
main = do
    command <- parseCommand <$> getArgs
    case command of
        DumbPlay gameCount rulesetName ->
            runDumbPlay gameCount rulesetName >>= putTextLn . renderDumbPlayStats
        Example rulesetName exampleMode -> runExample rulesetName exampleMode
        Service -> runService
        Help message -> do
            putTextLn message
            exitFailure
