module Game.CursedTreasure.APISpec where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Game.Core.Primitives (AdjacencyPolicy (NonAdjacency), TokenSpace (..))
import Game.CursedTreasure.API (censorHiddenInfo)
import Game.CursedTreasure.Types
    ( ClueObject(..)
    , ClueCard (HiddenClue, WithinStepsOf)
    , CensoredGameState (..)
    , Feature (Ocean)
    , GameState (..)
    , PlayerDescription (..)
    , PlayerId (..)
    , PlayerState (..)
    , RaisingTreasureState (..)
    , Score (CurrentScore)
    , TreasureCard (Curse, HiddenTreasure, Treasure)
    , allPlayerColors
    )

spec :: Spec
spec = do
    describe "censorHiddenInfo" $ do
        it "hides other players info, deck draw piles, and raising treasure chest" $ do
            let viewerId = PlayerId 1
                otherId = PlayerId 2

                (viewerColor, otherColor) =
                    case allPlayerColors of
                        (c1 : c2 : _) -> (c1, c2)
                        _ -> error "APISpec requires at least two player colors"

                viewerDesc =
                    PlayerDescription
                        { playerId = viewerId
                        , playerName = "Viewer"
                        , playerAI = ""
                        , playerColor = viewerColor
                        }

                otherDesc =
                    PlayerDescription
                        { playerId = otherId
                        , playerName = "Other"
                        , playerAI = ""
                        , playerColor = otherColor
                        }

                viewerClues = [WithinStepsOf 2 $ FeatureClue True Ocean]
                otherClues = [WithinStepsOf 3 $ FeatureClue True Ocean, WithinStepsOf 4 $ FeatureClue True Ocean]

                viewerTreasures = [Treasure 1, Curse]
                otherTreasures = [Curse, Treasure 2]

                mkPlayerState :: PlayerDescription -> [ClueCard] -> [TreasureCard] -> PlayerState
                mkPlayerState p cs ts =
                    PlayerState
                        { player = p
                        , clues = cs
                        , amulets = 0
                        , foundTreasures = []
                        , availableJeepMoves = 0
                        , availableCluePlays = 0
                        , availableRemoveMarkers = 0
                        , availablePickupAmulet = 0
                        , availableClueCardExchange = 0
                        , score = CurrentScore 0
                        , viewingTreasures = ts
                        }

                terrainBoard = TokenSpace {adjacency = NonAdjacency, tokens = Map.empty}

                g =
                    GameState
                        { players =
                            [ mkPlayerState viewerDesc viewerClues viewerTreasures
                            , mkPlayerState otherDesc otherClues otherTreasures
                            ]
                        , turn = 0
                        , playerTurn = viewerId
                        , activePlayer = viewerId
                        , clueDeck = ([WithinStepsOf 1 $ FeatureClue True Ocean, WithinStepsOf 2 $ FeatureClue True Ocean], [WithinStepsOf 9 $ FeatureClue True Ocean])
                        , treasureDeck = ([Treasure 9, Curse], [Treasure 99])
                        , terrainBoard = terrainBoard
                        , treasureBoards = []
                        , raisingTreasure =
                            Just
                                RaisingTreasureState
                                    { rtTreasureChest = ([Treasure 7, Curse], [])
                                    , rtOrder = [viewerId, otherId]
                                    , rtPlayerIndex = 0
                                    , rtViewing = []
                                    }
                        , latestMessage = ""
                        , gameOver = False
                        , seed = (0, 0)
                        }

                CensoredGameState g' = censorHiddenInfo g viewerId

            -- viewer's private info unchanged
            (g'.players !!? 0 <&> (.clues)) `shouldBe` Just viewerClues
            (g'.players !!? 0 <&> (.viewingTreasures)) `shouldBe` Just viewerTreasures

            -- other player's private info hidden (same lengths)
            (g'.players !!? 1 <&> (.clues))
                `shouldBe` Just (replicate (length otherClues) HiddenClue)
            (g'.players !!? 1 <&> (.viewingTreasures))
                `shouldBe` Just (replicate (length otherTreasures) HiddenTreasure)

            -- draw piles hidden; discards unchanged
            fst g'.clueDeck `shouldBe` replicate 2 HiddenClue
            snd g'.clueDeck `shouldBe` [WithinStepsOf 9 $ FeatureClue True Ocean]

            fst g'.treasureDeck `shouldBe` replicate 2 HiddenTreasure
            snd g'.treasureDeck `shouldBe` [Treasure 99]

            -- raising treasure chest hidden; rest unchanged
            fmap (\rt -> rt.rtTreasureChest) g'.raisingTreasure
                `shouldBe` Just ([Treasure 7, HiddenTreasure], [])
            fmap (\rt -> rt.rtOrder) g'.raisingTreasure `shouldBe` Just [viewerId, otherId]
            fmap (\rt -> rt.rtPlayerIndex) g'.raisingTreasure `shouldBe` Just 0
