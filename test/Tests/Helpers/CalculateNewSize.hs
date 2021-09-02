module Tests.Helpers.CalculateNewSize
  ( testCalculateNewSize
  ) where

import Helpers (calculateNewSize)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, choose, forAll)

triples :: Gen (Int, Int, Int)
triples = do
  x <- choose (100, 5000)
  y <- choose (100, 5000)
  z <- choose (100, 5000)
  return (x, y, z)

correctMaxSize :: Property
correctMaxSize =
  forAll
    triples
    (\(w, h, s) ->
       let (w', h') = calculateNewSize (w, h) s
        in max w' h' == s)

correctProportions :: Property
correctProportions =
  forAll
    triples
    (\(w, h, s) ->
       let (w', h') = calculateNewSize (w, h) s
        in (fromIntegral w' / fromIntegral h') -
           (fromIntegral w / fromIntegral h) <
           0.1)

testCalculateNewSize :: Spec
testCalculateNewSize = do
  describe "testing calculateNewSize function:" $ do
    prop "maximum image size equals size-parameter value" correctMaxSize
    prop "image proportions remains untouched" correctProportions
