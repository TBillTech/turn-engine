{-# OPTIONS_GHC -Wno-orphans #-}

module Game.CursedTreasure.Arbitrary () where

import Test.QuickCheck

import Game.Arbitrary ()
import Game.CursedTreasure.Types
    ( ClueCard (..)
    , ClueColor
    , ClueObject (..)
    , CensoredGameState
    , Feature (..)
    , GameMode (..)
    , GameState (..)
    , PlayerColor
    , PlayerDescription (..)
    , PlayerId
    , PlayerMove (..)
    , PlayerState (..)
    , RaisingTreasureState (..)
    , Score (..)
    , TerrainHex (..)
    , TerrainToken (..)
    , TreasureCard (..)
    , allClueColors
    , allPlayerIds
    , allPlayerColors
    , allFeatures
    , mkCensoredGameState
    )

instance Arbitrary PlayerColor where
    arbitrary = elements allPlayerColors

instance Arbitrary ClueColor where
    arbitrary = elements allClueColors

instance Arbitrary PlayerId where
    arbitrary = elements allPlayerIds

instance Arbitrary Feature where
    arbitrary = elements allFeatures

instance Arbitrary ClueObject where
    arbitrary =
        oneof
            [ FeatureClue <$> arbitrary <*> arbitrary
            , TokenClue <$> arbitrary
            , pure StatueClue
            ]

instance Arbitrary ClueCard where
    arbitrary =
        oneof
            [ WithinStepsOf <$> smallNonNegative <*> arbitrary
            , NotWithinStepsOf <$> smallNonNegative <*> arbitrary
            , IsOn <$> arbitrary
            , IsNotOn <$> arbitrary
            ]
      where
        smallNonNegative = chooseInt (1,2)

instance Arbitrary TreasureCard where
    arbitrary =
        oneof
            [ Treasure <$> smallNonNegative
            , pure Curse
            ]
      where
        smallNonNegative = chooseInt (1,6)

instance Arbitrary PlayerDescription where
    arbitrary =
        PlayerDescription
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary Score where
    arbitrary = oneof [CurrentScore <$> arbitrary, WinnerScore <$> arbitrary]

instance Arbitrary PlayerState where
    arbitrary =
        PlayerState
            <$> arbitrary
            <*> arbitrary
            <*> smallNonNegative
            <*> arbitrary
            <*> smallNonNegative
            <*> smallNonNegative
            <*> smallNonNegative
            <*> smallNonNegative
            <*> smallNonNegative
            <*> arbitrary
            <*> arbitrary
      where
        smallNonNegative = getNonNegative <$> (arbitrary :: Gen (NonNegative Int))

instance Arbitrary TerrainToken where
    arbitrary =
        oneof
            [ PlayerJeep <$> arbitrary
            , ClueToken <$> arbitrary
            , pure Amulet
            , pure Hut
            , pure PalmTree
            , Statue <$> arbitrary
            ]

instance Arbitrary TerrainHex where
    arbitrary = TerrainHex <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary RaisingTreasureState where
    arbitrary =
        RaisingTreasureState
            <$> arbitrary
            <*> arbitrary
            <*> smallNonNegative
            <*> arbitrary
      where
        smallNonNegative = getNonNegative <$> (arbitrary :: Gen (NonNegative Int))

instance Arbitrary GameState where
    arbitrary =
        GameState
            <$> arbitrary
            <*> smallNonNegative
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
      where
        smallNonNegative = getNonNegative <$> (arbitrary :: Gen (NonNegative Int))

instance Arbitrary CensoredGameState where
    arbitrary = mkCensoredGameState <$> arbitrary <*> arbitrary

instance Arbitrary PlayerMove where
    arbitrary =
        oneof
            [ pure PassTurn
            , PlayClue <$> arbitrary <*> arbitrary
            , MoveJeep <$> arbitrary <*> arbitrary
            , pure ExchangeClueCards
            , pure PickupAmulet
            , pure UseAmuletIncrMove
            , UseAmuletPlayClue <$> arbitrary <*> arbitrary
            , pure UseAmuletExchangeCards
            , UseAmuletRemoveSiteMarker <$> arbitrary <*> arbitrary <*> arbitrary
            , RaiseTreasure <$> arbitrary
            , pure RaisingTreasurePass
            , pure RaisingTreasureTake
            , pure RaisingTreasureWardCurse
            , pure RaisingTreasureAcceptCurse
            ]

instance Arbitrary GameMode where
    arbitrary =
        oneof
            [ pure GameModeNominal
            , pure GameModeMustMoveJeep
            , GameModeRaisingTreasureView <$> arbitrary
            , GameModeRaisingTreasureChoice <$> arbitrary
            ]