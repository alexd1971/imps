module Interpret.GD where

import Control.Monad.Free (Free (Free, Pure))
import DSL
  ( Imp,
    ImpF (DecodeJpeg, DecodePng, EncodeJpeg, EncodePng, ResizeImage, RotateToNormal),
    Orientation (CCW90, CW180, CW90, Normal),
    Size,
  )
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Dynamic (fromDynamic, toDyn)
import Graphics.GD
  ( Image,
    imageSize,
    loadJpegByteString,
    loadPngByteString,
    resizeImage,
    rotateImage,
    saveJpegByteString,
    savePngByteString,
  )
import Graphics.HsExif
  ( ImageOrientation (Rotation),
    RotationDirection (HundredAndEighty, MinusNinety, Ninety),
    getOrientation,
    parseExif,
  )
import Helpers

runGD :: Imp a -> IO a
runGD (Pure a) = return a
runGD (Free (DecodeJpeg bs f)) = do
  let eitherExif = parseExif bs
      orientation = case eitherExif of
        Left _ -> Normal
        Right exif ->
          let mo = getOrientation exif
           in case mo of
                Just (Rotation Ninety) -> CW90
                Just (Rotation MinusNinety) -> CCW90
                Just (Rotation HundredAndEighty) -> CW180
                _ -> Normal
  gdImage <- loadJpegByteString . toStrict $ bs
  runGD . f $ (toDyn gdImage, orientation)
runGD (Free (DecodePng bs f)) = do
  gdImage <- loadPngByteString . toStrict $ bs
  runGD . f $ toDyn gdImage
runGD (Free (EncodeJpeg quality img g)) = do
  let maybeGDImage = fromDynamic img :: Maybe Image
  case maybeGDImage of
    Just gdImage -> do
      bs <- saveJpegByteString quality gdImage
      runGD . g . fromStrict $ bs
    Nothing -> error "Cannot get image"
runGD (Free (EncodePng img g)) = do
  let maybeGDImage = fromDynamic img :: Maybe Image
  case maybeGDImage of
    Just gdImage -> do
      bs <- savePngByteString gdImage
      runGD . g . fromStrict $ bs
    Nothing -> error "Cannot get image"
runGD (Free (RotateToNormal orientation img f)) = do
  let maybeGDImage = fromDynamic img :: Maybe Image
  image <- case maybeGDImage of
    Just gdImage -> case orientation of
      CW90 -> rotateImage 1 gdImage
      CCW90 -> rotateImage 3 gdImage
      CW180 -> rotateImage 2 gdImage
      _ -> return gdImage
    Nothing -> error "Cannot get image"
  runGD . f . toDyn $ image
runGD (Free (ResizeImage size img f)) = do
  let maybeGDImage = fromDynamic img :: Maybe Image
  case maybeGDImage of
    Just gdImage -> do
      resizedImage <- gdResize size gdImage
      runGD . f $ toDyn resizedImage
    Nothing -> error "Cannot get image"

-- Makes image resize with GD library
gdResize :: Size -> Image -> IO Image
gdResize requestedSize image = do
  actualSize <- imageSize image
  let newSize = calculateNewSize actualSize requestedSize
  uncurry resizeImage newSize image
