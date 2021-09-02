module Tests.MockImageLibrary
  ( testEncodeDecode
  , testRotate
  , testResize
  , testMockImageLibrary
  ) where

import DSL.ImpLang (Orientation(CCW90, CW90, Normal, UpSideDown))
import MockImageLibrary
  ( Direction(..)
  , Height
  , MockImage(height, orientation, width)
  , Width
  , decode
  , encode
  , resize
  , rotate
  )
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(arbitrary), Gen, Property, choose, forAll)

encodeDecode :: MockImage -> Bool
encodeDecode image = (decode . encode) image == image

widthAndHeightSwaps :: Direction -> MockImage -> Bool
widthAndHeightSwaps direction image =
  let image' = rotate direction image
   in height image == width image' && width image == height image'

correctOrientation :: Direction -> MockImage -> Bool
correctOrientation direction image =
  let image' = rotate direction image
      oldOrientation = orientation image
      newOrientation = orientation image'
   in case direction of
        CW ->
          case oldOrientation of
            Normal -> newOrientation == CW90
            CW90 -> newOrientation == UpSideDown
            UpSideDown -> newOrientation == CCW90
            CCW90 -> newOrientation == Normal
        CCW ->
          case oldOrientation of
            Normal -> newOrientation == CCW90
            CW90 -> newOrientation == Normal
            UpSideDown -> newOrientation == CW90
            CCW90 -> newOrientation == UpSideDown

testEncodeDecode :: Spec
testEncodeDecode = do
  prop "decode of encoded image" encodeDecode

testRotate :: Spec
testRotate = do
  prop "rotation swaps width and height" widthAndHeightSwaps
  prop "rotation changes orientation correctly" correctOrientation

resizeProp' :: Width -> Height -> MockImage -> Bool
resizeProp' w h image =
  let image' = resize w h image
   in width image' == w && height image' == h

resizeParams :: Gen (Int, Int, MockImage)
resizeParams = do
  w <- choose (100, 5000)
  h <- choose (100, 5000)
  image <- arbitrary
  return (w, h, image)

resizeProp :: Property
resizeProp = forAll resizeParams (\(w, h, image) -> resizeProp' w h image)

testResize :: Spec
testResize = do
  prop "resizing changes image size correctly" resizeProp

testMockImageLibrary :: Spec
testMockImageLibrary = do
  describe "testing encode and decode functions:" testEncodeDecode
  describe "testing rotate function:" testRotate
  describe "testing resize function" testResize
