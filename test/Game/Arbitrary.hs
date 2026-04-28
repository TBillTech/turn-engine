{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Arbitrary () where

import qualified Data.Map.Strict as Map
import Test.QuickCheck

import Game.Core.Primitives
    ( GameColor
    , HourHand
    , allGameColors
    , toHourHand
    , CubeCoordinate
    , mkCubeCoordinate
    , CubeCoordinateTokens (..)
    )

instance Arbitrary Text where
    arbitrary = toText <$> (arbitrary :: Gen String)

instance Arbitrary GameColor where
    arbitrary = elements allGameColors

instance Arbitrary (CubeCoordinate Int) where
    arbitrary = do
        q <- chooseInt (-50, 50)
        r <- chooseInt (-50, 50)
        pure $ mkCubeCoordinate q r

instance Arbitrary t => Arbitrary (CubeCoordinateTokens Int t) where
    arbitrary = do
        h <- arbitrary
        CubeCoordinateTokens h . Map.fromList <$> arbitrary

instance Arbitrary HourHand where
    arbitrary = toHourHand <$> chooseInt (0, 11)
