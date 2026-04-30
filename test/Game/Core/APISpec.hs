module Game.Core.APISpec where

import Test.Hspec

import qualified Game.ArtOfWar.Types as ArtOfWar
import qualified Game.Core.API as Core
import Game.Core.Types
import qualified Game.CursedTreasure.API as CursedTreasure
import qualified Game.CursedTreasure.Types as CursedTreasure

spec :: Spec
spec = do
    describe "getGameSetupPlayers" $ do
        it "returns one player setup list per ruleset" $ do
            let allSetups = Core.getGameSetupPlayers

            length allSetups `shouldBe` 4
            allSetups `shouldSatisfy` any (not . null)

    describe "createNewGame" $ do
        it "rejects an empty player list because the ruleset cannot be inferred" $ do
            Core.createNewGame [] 0
                `shouldBe` Left "PlayerDescription list must be non-empty and all match the same ruleset."

        it "rejects mixed player description rulesets" $ do
            let cursedTreasurePlayer =
                    case CursedTreasure.getGameSetupPlayers of
                        player : _ -> player
                        [] -> error "expected Cursed Treasure setup player"
                mixedPlayers =
                    [ CursedTreasurePlayerDescription cursedTreasurePlayer
                    , ArtOfWarPlayerDescription (ArtOfWar.PlayerDescription "Player")
                    ]

            Core.createNewGame mixedPlayers 0
                `shouldBe` Left "PlayerDescription values must all match the same ruleset."

    describe "CursedTreasure delegation" $ do
        it "delegates enumerateActivePlayerOptions and makeMove" $ do
            let players = take 2 CursedTreasure.getGameSetupPlayers
                wrappedPlayers = map CursedTreasurePlayerDescription players
                coreState =
                    case Core.createNewGame wrappedPlayers 12345 of
                        Right (gameState, _) -> gameState
                        Left err -> error $ "expected CursedTreasure game creation to succeed: " <> show err
                cursedState = case coreState of
                    CursedTreasureGame gameState -> gameState
                    _ -> error "Expected CursedTreasureGame"
                coreMoves = Core.enumerateActivePlayerOptions coreState
                cursedMoves = CursedTreasure.enumerateActivePlayerOptions cursedState

            coreMoves `shouldBe` map CursedTreasurePlayerMove cursedMoves

            case (coreMoves, cursedMoves) of
                (coreMove : _, cursedMove : _) ->
                    Core.makeMove coreState coreMove
                        `shouldBe`
                        wrapCursedResult (CursedTreasure.makeMove cursedState cursedMove)
                _ -> expectationFailure "Expected Cursed Treasure to have at least one legal move"
wrapCursedResult :: (CursedTreasure.GameState, [CursedTreasure.CensoredGameState]) -> (GameState, [CensoredGameState])
wrapCursedResult (gameState, censoredStates) =
    ( CursedTreasureGame gameState
    , map CursedTreasureCensoredGameState censoredStates
    )