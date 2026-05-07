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

        it "parses --move_example_request with a ruleset and move constructor" $ do
            parseCommand ["--move_example_request", "CursedTreasure", "RaisingTreasureWardCurse"]
                `shouldBe` MoveExampleRequest CursedTreasure "RaisingTreasureWardCurse"