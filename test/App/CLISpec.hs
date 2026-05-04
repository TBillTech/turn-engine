module App.CLISpec where

import Test.Hspec

import App.CLI

spec :: Spec
spec = do
    describe "parseCommand" $ do
        it "parses --version" $ do
            parseCommand ["--version"] `shouldBe` Version

        it "parses --test-example with request and response files" $ do
            parseCommand ["--test-example", "docs/request.json", "docs/response.json"]
                `shouldBe` TestExample "docs/request.json" "docs/response.json"