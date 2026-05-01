module App.Service
    ( handleRequest
    , runService
    )
where

import Data.Aeson (decodeStrict', encode)
import qualified Data.ByteString.Char8 as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString
import System.IO (stdin, stdout)

import qualified Game.Core.API as Core

import App.Protocol

handleRequest :: ServiceRequest -> ServiceResponse
handleRequest = \case
    GetGameSetupPlayers -> GameSetupPlayers Core.getGameSetupPlayers
    CreateNewGame players randomSeed -> NewGameCreated (Core.createNewGame players randomSeed)
    EnumerateActivePlayerOptions gameState -> ActivePlayerOptions (Core.enumerateActivePlayerOptions gameState)
    MakeMove gameState playerMove -> MoveApplied (Core.makeMove gameState playerMove)
    HeuristicHint level gameState playerMoves -> HintGenerated (Core.heuristicHint level gameState playerMoves)
    Summary gameState -> SummaryGenerated (Core.summary gameState)

runService :: IO ()
runService = loop
  where
    loop = do
        isDone <- hIsEOF stdin
        unless isDone $ do
            inputLine <- StrictByteString.hGetLine stdin
            StrictByteString.putStrLn (encodeResponse inputLine)
            hFlush stdout
            loop

encodeResponse :: StrictByteString.ByteString -> StrictByteString.ByteString
encodeResponse inputLine =
    LazyByteString.toStrict . encode $
        case decodeStrict' inputLine of
            Nothing -> ServiceError "Invalid request JSON."
            Just request -> handleRequest request