module Game.Core.PrimitivesSpec where

import Game.Core.Primitives
import Test.Hspec

spec :: Spec
spec = do
    describe "toXYScaledOrientation" $ do
        mapM_ projectionSpec projectionCases

    describe "cubeCoordinateDistance" $ do
        it "is zero at the same coordinate" $ do
            cubeCoordinateDistance origin origin `shouldBe` 0

        it "assigns distance 1 to every adjacent hex" $ do
            mapM_ (\coord -> cubeCoordinateDistance origin (uncurry mkCubeCoordinate coord) `shouldBe` 1)
                (filter (/= (0, 0)) radiusOneCubeCoordinates)

        it "matches explicit Euclidean distances on the axial basis" $ do
            cubeCoordinateDistance origin (mkCubeCoordinate 2 0) `shouldBe` 2
            cubeCoordinateDistance origin (mkCubeCoordinate 1 1) `shouldApproxScalarBe` sqrt 3
            cubeCoordinateDistance origin (mkCubeCoordinate 2 (-2)) `shouldBe` 2
            cubeCoordinateDistance (mkCubeCoordinate (1 :: Int) 0) (mkCubeCoordinate 0 1) `shouldBe` 1

    describe "adjacentCubeCoordinates" $ do
        it "returns the six radius-1 offsets translated to the target coordinate" $ do
            adjacentCubeCoordinates center `shouldMatchList` expectedAdjacentCoordinates

projectionCases :: [(Int, (Double, Double), (Double, Double))]
projectionCases =
    [ (0, (0, 1), (negate halfSqrt3, 0.5))
    , (3, (1, 0), (0.5, halfSqrt3))
    , (6, (0, -1), (halfSqrt3, -0.5))
    , (9, (-1, 0), (-0.5, negate halfSqrt3))
    ]

projectionSpec :: (Int, (Double, Double), (Double, Double)) -> Spec
projectionSpec (hour, qBasis, rBasis) =
    describe ("for HourHand " ++ show hour) $ do
        it "projects the origin and every radius-1 hex explicitly" $ do
            mapM_ (assertProjection hour qBasis rBasis) radiusOneCubeCoordinates

        it "projects every radius-2 hex explicitly" $ do
            mapM_ (assertProjection hour qBasis rBasis) radiusTwoCubeCoordinates

assertProjection :: Int -> (Double, Double) -> (Double, Double) -> (Int, Int) -> Expectation
assertProjection hour qBasis rBasis (q, r) = do
    let actual = toXYScaledOrientation (toHourHand hour) 1 (mkCubeCoordinate q r)
        expected = combineExpected qBasis rBasis (fromIntegral q) (fromIntegral r)
    actual `shouldApproxBe` expected

combineExpected :: (Double, Double) -> (Double, Double) -> Double -> Double -> (Double, Double)
combineExpected (qx, qy) (rx, ry) q r =
    (q * qx + r * rx, q * qy + r * ry)

shouldApproxBe :: (Double, Double) -> (Double, Double) -> Expectation
shouldApproxBe (actualX, actualY) (expectedX, expectedY) = do
    actualX `shouldSatisfy` closeTo expectedX
    actualY `shouldSatisfy` closeTo expectedY
    where closeTo expected actual = abs (actual - expected) < 1.0e-9

halfSqrt3 :: Double
halfSqrt3 = sqrt 3 / 2

origin :: CubeCoordinate Int
origin = mkCubeCoordinate 0 0

shouldApproxScalarBe :: Double -> Double -> Expectation
shouldApproxScalarBe actual expected =
    actual `shouldSatisfy` closeTo expected
    where closeTo target value = abs (value - target) < 1.0e-9

center :: CubeCoordinate Int
center = mkCubeCoordinate 2 (-1)

expectedAdjacentCoordinates :: [CubeCoordinate Int]
expectedAdjacentCoordinates =
    [ mkCubeCoordinate 3 (-1)
    , mkCubeCoordinate 2 0
    , mkCubeCoordinate 1 0
    , mkCubeCoordinate 1 (-1)
    , mkCubeCoordinate 2 (-2)
    , mkCubeCoordinate 3 (-2)
    ]