module App.TestExample
    ( runTestExample
    , validateExamplePair
    )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import qualified Data.ByteString as StrictByteString

import App.Protocol (ServiceResponse)
import App.Service (handleRequest)

runTestExample :: FilePath -> FilePath -> IO (Either Text ())
runTestExample requestFilePath responseFilePath = do
    requestJson <- readFileBS requestFilePath
    responseJson <- readFileBS responseFilePath
    pure (validateExamplePair requestFilePath responseFilePath requestJson responseJson)

validateExamplePair :: FilePath -> FilePath -> StrictByteString.ByteString -> StrictByteString.ByteString -> Either Text ()
validateExamplePair requestFilePath responseFilePath requestJson responseJson = do
    request <- firstDecodeError requestFilePath "request" requestJson
    expectedResponse <- firstDecodeError responseFilePath "response" responseJson
    let actualResponse = handleRequest request
    if actualResponse == expectedResponse
        then Right ()
        else Left (renderMismatch responseFilePath expectedResponse actualResponse)

firstDecodeError :: FromJSON a => FilePath -> Text -> StrictByteString.ByteString -> Either Text a
firstDecodeError filePath label jsonBytes =
    case eitherDecodeStrict' jsonBytes of
        Left err -> Left ("Failed to decode " <> label <> " JSON from " <> toText filePath <> ": " <> toText err)
        Right value -> Right value

renderMismatch :: FilePath -> ServiceResponse -> ServiceResponse -> Text
renderMismatch responseFilePath expectedResponse actualResponse =
    unlines
        [ "Response mismatch for " <> toText responseFilePath <> "."
        , "Expected:"
        , renderCanonicalJson expectedResponse
        , "Actual:"
        , renderCanonicalJson actualResponse
        ]

renderCanonicalJson :: ToJSON a => a -> Text
renderCanonicalJson = decodeUtf8 . StrictByteString.toStrict . encode