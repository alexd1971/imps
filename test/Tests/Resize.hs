{-# LANGUAGE RecordWildCards #-}

module Tests.Resize where

--   ( testJpegResize
--   , testPngResize
--   , testImageResize
--   ) where
import DSL.ImpLang (Height, Orientation(..), Quality(..), Width)
import Data.ByteString.Lazy (fromStrict, toStrict)
import MockImageLibrary (MockImage(..), decode, encode)
import Scripts.Resize
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, arbitrary, choose, forAll)
import Tests.Interpreters.ImpLang (run)

data JpegData =
  JpegData
    { image :: MockImage
    , mw :: Maybe Width
    , mh :: Maybe Height
    , quality :: Quality
    }

jpegResizeData :: Gen JpegData
jpegResizeData = do
  image <- arbitrary
  w <- arbitrary :: Gen Int
  h <- arbitrary :: Gen Int
  q <- choose (-1, 100)
  let mw =
        case w < 100 of
          True -> Nothing
          _ -> Just w
      mh =
        case h < 100 of
          True -> Nothing
          _ -> Just h
      quality =
        case q of
          -1 -> Default
          _ -> Quality q
  return JpegData {..}
-- correctJpegSize :: Property
-- correctJpegSize =
--   forAll
--     jpegResizeData
--     (\(image, s, q) ->
--        let bs = fromStrict . encode $ image
--            image' = decode $ toStrict . run $ imageJpegResize bs s q
--         in (width image', height image') == s)
-- correctJpegQuality :: Property
-- correctJpegQuality =
--   forAll
--     jpegResizeData
--     (\(image, s, q) ->
--        let bs = fromStrict . encode $ image
--            image' = decode $ toStrict . run $ imageJpegResize bs s q
--         in quality image' == q)
-- correctJpegOrientation :: Property
-- correctJpegOrientation =
--   forAll
--     jpegResizeData
--     (\(image, s, q) ->
--        let bs = fromStrict . encode $ image
--            image' = decode $ toStrict . run $ imageJpegResize bs s q
--         in orientation image' == Normal)
-- pngResizeData :: Gen (MockImage, Size)
-- pngResizeData = do
--   image <- arbitrary
--   w <- choose (100, 5000)
--   h <- choose (100, 5000)
--   return (image, (w, h))
-- correctPngSize :: Property
-- correctPngSize =
--   forAll
--     pngResizeData
--     (\(image, s) ->
--        let bs = fromStrict . encode $ image
--            image' = decode $ toStrict . run $ imagePngResize bs s
--         in (width image', height image') == s)
-- testJpegResize :: Spec
-- testJpegResize = do
--   prop "correct image size" correctJpegSize
--   prop "correct JPEG image quality" correctJpegQuality
--   prop "correct JPEG image orientation" correctJpegOrientation
-- testPngResize :: Spec
-- testPngResize = do
--   prop "correct PNG image size" correctPngSize
-- testImageResize :: Spec
-- testImageResize = do
--   describe "testing JPEG image resize:" testJpegResize
--   describe "testing PNG image resize:" testPngResize
