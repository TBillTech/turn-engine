module LibSpec where

import Test.Hspec
import Test.QuickCheck

import Lib

spec :: Spec
spec = do
    describe "add" $ do
        it "is commutative" $
            property $ \x y -> add x y === add (y :: Int) (x :: Int)
        it "is associative" $
            property $ \x y z ->
                add x (add y z) === add (add (x :: Int) y) z
        it "has zero as identity" $
            property $ \x -> add (x :: Int) 0 === x
