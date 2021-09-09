module Tests.MockImageLibrary
  ( testEncodeDecode
  , testRotate
  , testResize
  , testMockImageLibrary
  ) where

import DSL.ImpLang (Orientation(CCW90, CW90, Normal, UpSideDown), Size)
import MockImageLibrary
  ( Direction(..)
  , MockImage(height, orientation, width)
  , crop
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

type Method = Size -> MockImage -> MockImage

resizeProp' :: Method -> Size -> MockImage -> Bool
resizeProp' method s@(w, h) image =
  let image' = method s image
   in width image' == w && height image' == h

resizeParams :: Gen (Size, MockImage)
resizeParams = do
  w <- choose (100, 5000)
  h <- choose (100, 5000)
  image <- arbitrary
  return ((w, h), image)

resizeProp :: Property
resizeProp = forAll resizeParams (uncurry $ resizeProp' resize)

cropProp :: Property
cropProp = forAll resizeParams (uncurry $ resizeProp' crop)

testResize :: Spec
testResize = do
  prop "resizing changes image size correctly" resizeProp

testCrop :: Spec
testCrop = do
  prop "cropping changes image size correctly" cropProp

testMockImageLibrary :: Spec
testMockImageLibrary = do
  describe "testing encode and decode functions:" testEncodeDecode
  describe "testing rotate function:" testRotate
  describe "testing resize function" testResize
  describe "testing crop function" testCrop
