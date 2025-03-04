module Unit.Plucker (tests) where
import Linear
import Linear.Plucker
import Linear.Plucker.Coincides
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=), testCase)

ln2,ln3,ln4,ln5,ln6,ln7,ln8,ln9 :: Plucker Float
ln2 = plucker3D (V3 1 3 0) (V3 1 3 (-2))    -- starting line
ln3 = plucker3D (V3 2 3 0) (V3 2 3 (-2))    -- parallel
ln4 = plucker3D (V3 2 4 0) (V3 1 4 (-2))    -- ccw
ln5 = plucker3D (V3 (-2) 4 0) (V3 2 4 (-2)) -- cw
ln6 = plucker3D (V3 2 3 0) (V3 1 3 (-2))    -- intersect
ln7 = plucker3D (V3 1 3 0) (V3 1 3 2)       -- reversed
ln8 = plucker3D (V3 0 4 4) (V3 0 (-4) (-4)) -- through origin
ln9 = Plucker 1 2 3 4 5 6                   -- not a 3D line

tests :: [TestTree]
tests = [ testCase "parallel" $ parallel ln2 ln3 @?= True
        , testCase "CCW" $ passes ln2 ln4 @?= Counterclockwise
        , testCase "CW" $ passes ln2 ln5 @?= Clockwise
        , testCase "intersect1" $ intersects ln2 ln6 @?= True
        , testCase "intersect2" $ intersects ln2 ln3 @?= False
        , testCase "line equality 1" $ Line ln2 == Line ln2 @?= True
        , testCase "line equality 2" $ Line ln2 == Line ln7 @?= True
        , testCase "line equality 3" $ Line ln2 == Ray ln7 @?= True
        , testCase "line equality 4" $ Ray ln2 == Line ln7 @?= True
        , testCase "ray equality 1" $ Ray ln2 == Ray ln7 @?= False
        , testCase "ray equality 2" $ Ray ln2 == Ray (3 *^ ln2) @?= True
        , testCase "ray equality 3" $ Ray ln2 == Ray (negate ln7) @?= True
        , testCase "quadrance" $ nearZero (quadranceToOrigin ln2 - 10) @?= True
        , testCase "closest 1" $
            nearZero (qd (V3 1 3 0) $ closestToOrigin ln2) @?= True
        , testCase "closest 2" $ nearZero (qd 0 $ closestToOrigin ln8) @?= True
        , testCase "isLine 1" $ isLine ln2 @?= True
        , testCase "isLine 2" $ isLine ln9 @?= False ]
