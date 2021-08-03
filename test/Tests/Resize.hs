module Tests.Resize
  ( testJpegResize,
    testPngResize,
    testImageResize,
  )
where

import DSL
import Data.ByteString.Lazy
import Interpreters.TestInterpreter
import MockImageLibrary
import Resize
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

jpegResizeData :: Gen (MockImage, Size, Quality)
jpegResizeData = do
  image <- arbitrary
  size <- choose (100, 5000)
  quality <- choose (0, 100)
  return (image, size, quality)

correctJpegSize :: Property
correctJpegSize =
  forAll
    jpegResizeData
    ( \(image, s, q) ->
        let bs = fromStrict . encode $ image
            image' = decode $ toStrict . run $ imageJpegResize bs s q
         in max (width image') (height image') == s
    )

correctJpegQuality :: Property
correctJpegQuality =
  forAll
    jpegResizeData
    ( \(image, s, q) ->
        let bs = fromStrict . encode $ image
            image' = decode $ toStrict . run $ imageJpegResize bs s q
         in quality image' == q
    )

correctJpegOrientation :: Property
correctJpegOrientation =
  forAll
    jpegResizeData
    ( \(image, s, q) ->
        let bs = fromStrict . encode $ image
            image' = decode $ toStrict . run $ imageJpegResize bs s q
         in orientation image' == Normal
    )

pngResizeData :: Gen (MockImage, Size)
pngResizeData = do
  image <- arbitrary
  size <- choose (100, 5000)
  return (image, size)

correctPngSize :: Property
correctPngSize =
  forAll
    pngResizeData
    ( \(image, s) ->
        let bs = fromStrict . encode $ image
            image' = decode $ toStrict . run $ imagePngResize bs s
         in max (width image') (height image') == s
    )

testJpegResize :: Spec
testJpegResize = do
  prop "correct image size" correctJpegSize
  prop "correct JPEG image quality" correctJpegQuality
  prop "correct JPEG image orientation" correctJpegOrientation

testPngResize :: Spec
testPngResize = do
  prop "correct PNG image size" correctPngSize

testImageResize :: Spec
testImageResize = do
  describe "testing JPEG image resize:" testJpegResize
  describe "testing PNG image resize:" testPngResize
