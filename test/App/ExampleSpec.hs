module App.ExampleSpec where

import Test.Hspec

import App.CLI (RulesetName (CursedTreasure))
import App.Example (buildMoveExampleRequest)
import App.Protocol (ServiceRequest (MakeMove), ServiceResponse (MoveApplied))
import App.Service (handleRequest)
import qualified Game.Core.Types as Core
import qualified Game.CursedTreasure.API as CursedTreasure
import qualified Game.CursedTreasure.Types as CursedTreasure

spec :: Spec
spec = do
    describe "buildMoveExampleRequest" $ do
        mapM_ assertLegalMoveExample allConstructorNames

assertLegalMoveExample :: Text -> SpecWith ()
assertLegalMoveExample moveName =
    it ("builds a usable example request for " <> toString moveName) $ do
        case buildMoveExampleRequest CursedTreasure moveName of
            Left err -> expectationFailure (toString err)
            Right (MakeMove (Core.CursedTreasureGame gameState) (Core.CursedTreasurePlayerMove playerMove)) -> do
                case moveName of
                    "PlayerMoveError" -> playerMove `shouldBe` CursedTreasure.PlayerMoveError "example move error"
                    _ -> CursedTreasure.enumerateActivePlayerOptions gameState `shouldContain` [playerMove]
                handleRequest (MakeMove (Core.CursedTreasureGame gameState) (Core.CursedTreasurePlayerMove playerMove))
                    `shouldSatisfy` isMoveApplied
            Right request -> expectationFailure ("Expected a CursedTreasure makeMove request, got: " <> show request)

isMoveApplied :: ServiceResponse -> Bool
isMoveApplied (MoveApplied _) = True
isMoveApplied _ = False

allConstructorNames :: [Text]
allConstructorNames =
    [ "PlayerMoveError"
    , "PassTurn"
    , "PlayClue"
    , "MoveJeep"
    , "ExchangeClueCards"
    , "PickupAmulet"
    , "UseAmuletIncrMove"
    , "UseAmuletPlayClue"
    , "UseAmuletExchangeCards"
    , "UseAmuletRemoveSiteMarker"
    , "RaiseTreasure"
    , "RaisingTreasurePass"
    , "RaisingTreasureTake"
    , "RaisingTreasureWardCurse"
    , "RaisingTreasureAcceptCurse"
    ]