module Main where

import Data.Version (showVersion)
import GHC.IO.Encoding (utf8)
import System.IO (hSetEncoding)

import App.CLI (Command (..), parseCommand)
import App.DumbPlay (renderDumbPlayStats, runDumbPlay)
import App.Example (runExample)
import App.Service (runService)
import App.TestExample (runTestExample)
import qualified Paths_turn_engine

main :: IO ()
main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    command <- parseCommand <$> getArgs
    case command of
        DumbPlay gameCount rulesetName ->
            runDumbPlay gameCount rulesetName >>= putTextLn . renderDumbPlayStats
        Example rulesetName exampleMode -> runExample rulesetName exampleMode
        Service -> runService
        Version -> putTextLn (toText (showVersion Paths_turn_engine.version))
        TestExample requestFilePath responseFilePath -> do
            result <- runTestExample requestFilePath responseFilePath
            case result of
                Left message -> do
                    putTextLn message
                    exitFailure
                Right () -> pure ()
        Help message -> do
            putTextLn message
            exitFailure
