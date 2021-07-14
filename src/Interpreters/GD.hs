{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreters.GD (runGD) where

import Control.Monad.Free (Free (Free, Pure))
import DSL
  ( Img (..),
    Imp,
    Interpreter (onDecodeJpeg, onDecodePng, onEncodeJpeg, onEncodePng, onResizeImage, onRotateToNormal),
    Orientation (CCW90, CW180, CW90, Normal),
    Size,
    interpret,
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
import qualified Graphics.HsExif as Exif
  ( ImageOrientation (Normal, Rotation),
    RotationDirection (HundredAndEighty, MinusNinety, Ninety),
    getOrientation,
    parseExif,
  )
import Helpers

instance Interpreter Image IO where
  onDecodeJpeg bs = do
    let eitherExif = Exif.parseExif bs
        orientation = case eitherExif of
          Left _ -> Normal
          Right exif ->
            let mo = Exif.getOrientation exif
             in case mo of
                  Just (Exif.Rotation Exif.Ninety) -> CW90
                  Just (Exif.Rotation Exif.MinusNinety) -> CCW90
                  Just (Exif.Rotation Exif.HundredAndEighty) -> CW180
                  _ -> Normal
    gdImage <- loadJpegByteString . toStrict $ bs
    return (Img gdImage, orientation)

  onDecodePng bs = Img <$> loadPngByteString (toStrict bs)

  onEncodeJpeg quality (Img gdImage) = fromStrict <$> saveJpegByteString quality gdImage

  onEncodePng (Img gdImage) = fromStrict <$> savePngByteString gdImage

  onRotateToNormal orientation (Img gdImage) =
    Img <$> case orientation of
      CW90 -> rotateImage 1 gdImage
      CCW90 -> rotateImage 3 gdImage
      CW180 -> rotateImage 2 gdImage
      _ -> return gdImage

  onResizeImage size (Img gdImage) = Img <$> gdResize size gdImage

-- Makes image resize with GD library
gdResize :: Size -> Image -> IO Image
gdResize requestedSize image = do
  actualSize <- imageSize image
  let newSize = calculateNewSize actualSize requestedSize
  uncurry resizeImage newSize image

runGD :: Imp Image a -> IO a
runGD = interpret
