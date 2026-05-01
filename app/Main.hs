module Main where

import App.CLI (Command (..), parseCommand)
import App.DumbPlay (renderDumbPlayStats, runDumbPlay)
import App.Service (runService)

main :: IO ()
main = do
    command <- parseCommand <$> getArgs
    case command of
        DumbPlay gameCount rulesetName ->
            runDumbPlay gameCount rulesetName >>= putTextLn . renderDumbPlayStats
        Service -> runService
        Help message -> do
            putTextLn message
            exitFailure
