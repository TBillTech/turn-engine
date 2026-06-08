module App.TestExampleSpec where

import qualified Data.ByteString.Char8 as StrictByteString
import Data.List (isInfixOf)
import Test.Hspec

import App.TestExample (validateExamplePair)

spec :: Spec
spec = do
    describe "validateExamplePair" $ do
        it "accepts a matching request and response pair" $ do
            validateExamplePair "request.json" "response.json" getGameSetupPlayersRequest gameSetupPlayersResponse
                `shouldBe` Right ()

        it "fails with a specific decode error for invalid request JSON" $ do
            validateExamplePair "request.json" "response.json" "{" gameSetupPlayersResponse
                `shouldSatisfy` isLeftContaining "Failed to decode request JSON from request.json"

        it "fails when the response does not match engine output" $ do
            validateExamplePair "request.json" "response.json" getGameSetupPlayersRequest serviceErrorResponse
                `shouldSatisfy` isLeftContaining "Response mismatch for response.json"

getGameSetupPlayersRequest :: StrictByteString.ByteString
getGameSetupPlayersRequest = "{\"request\":\"getGameSetupPlayers\"}"

gameSetupPlayersResponse :: StrictByteString.ByteString
gameSetupPlayersResponse = "{\"response\":\"gameSetupPlayers\",\"rulesets\":[{\"name\":\"Cursed Treasure\",\"playerTemplates\":[{\"playerRuleset\":\"Cursed Treasure\",\"playerId\":1,\"playerName\":\"Player PlayerId 1\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Red\"},{\"playerRuleset\":\"Cursed Treasure\",\"playerId\":2,\"playerName\":\"Player PlayerId 2\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Green\"},{\"playerRuleset\":\"Cursed Treasure\",\"playerId\":3,\"playerName\":\"Player PlayerId 3\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Blue\"},{\"playerRuleset\":\"Cursed Treasure\",\"playerId\":4,\"playerName\":\"Player PlayerId 4\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Purple\"}]},{\"name\":\"Fog Of Battle\",\"playerTemplates\":[]},{\"name\":\"Art Of War\",\"playerTemplates\":[]},{\"name\":\"Real Estate\",\"playerTemplates\":[]}]}"

serviceErrorResponse :: StrictByteString.ByteString
serviceErrorResponse = "{\"response\":\"serviceError\",\"message\":\"Invalid request JSON.\"}"

isLeftContaining :: String -> Either Text () -> Bool
isLeftContaining expectedText = \case
    Left err -> expectedText `isInfixOf` toString err
    Right () -> False