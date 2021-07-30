{-# LANGUAGE TypeFamilies #-}

module Interpreters.GD (runGD) where

import DSL
  ( Image,
    ImpScript,
    Interpreter (..),
    Orientation (..),
    Size,
    interpret,
  )
import Data.ByteString.Lazy
  ( ByteString,
    fromStrict,
    toStrict,
  )
import qualified Graphics.GD as GD
  ( Image,
    imageSize,
    loadJpegByteString,
    loadPngByteString,
    resizeImage,
    rotateImage,
    saveJpegByteString,
    savePngByteString,
  )
import qualified Graphics.HsExif as Exif
  ( ImageOrientation (Normal, Rotation),
    RotationDirection (HundredAndEighty, MinusNinety, Ninety),
    getOrientation,
    parseExif,
  )
import Helpers

type instance Image = GD.Image

instance Interpreter IO where
  onDecodeJpeg bs = do
    let eitherExif = Exif.parseExif bs
        orientation = case eitherExif of
          Left _ -> Normal
          Right exif ->
            let maybeOrientation = Exif.getOrientation exif
             in case maybeOrientation of
                  Just (Exif.Rotation Exif.Ninety) -> CW90
                  Just (Exif.Rotation Exif.MinusNinety) -> CCW90
                  Just (Exif.Rotation Exif.HundredAndEighty) -> UpSideDown
                  _ -> Normal
    image <- GD.loadJpegByteString . toStrict $ bs
    return (image, orientation)

  onDecodePng bs = GD.loadPngByteString (toStrict bs)

  onEncodeJpeg quality image = fromStrict <$> GD.saveJpegByteString quality image

  onEncodePng image = fromStrict <$> GD.savePngByteString image

  onRotateToNormal orientation image = case orientation of
    CW90 -> GD.rotateImage 1 image
    CCW90 -> GD.rotateImage 3 image
    UpSideDown -> GD.rotateImage 2 image
    _ -> return image

  onResizeImage size img = do
    actualSize <- GD.imageSize img
    let newSize = calculateNewSize actualSize size
    uncurry GD.resizeImage newSize img

runGD :: ImpScript a -> IO a
runGD = interpret
