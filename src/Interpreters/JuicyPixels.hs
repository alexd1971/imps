{-# LANGUAGE TypeFamilies #-}

module Interpreters.JuicyPixels (runJuicyPixels) where

import qualified Codec.Picture as JP
  ( Image (imageHeight, imageWidth),
    PixelRGB8,
    convertRGB8,
    decodeImage,
    decodeImageWithMetadata,
    encodeJpeg,
    encodeJpegAtQuality,
    encodePng,
  )
import Codec.Picture.Extra
  ( rotate180,
    rotateLeft90,
    rotateRight90,
    scaleBilinear,
  )
import Codec.Picture.Metadata (extractExifMetas)
import Codec.Picture.Metadata.Exif
  ( ExifData (ExifShort),
    ExifTag (TagOrientation),
  )
import Codec.Picture.Types (ColorSpaceConvertible (convertImage))
import Control.Monad.Free (Free (Free, Pure))
import DSL
  ( Image,
    ImpScript,
    Interpreter (..),
    Orientation (..),
    interpret,
  )
import Data.ByteString.Lazy (toStrict)
import Helpers (calculateNewSize)

type instance Image = JP.Image JP.PixelRGB8

instance Interpreter IO where
  onDecodeJpeg bs = do
    return (image, orientation)
    where
      (image, metadata) = case JP.decodeImageWithMetadata . toStrict $ bs of
        Right (img, meta) -> (JP.convertRGB8 img, meta)
        Left e -> error e
      orientation = case lookup TagOrientation . extractExifMetas $ metadata of
        Just (ExifShort v) -> case v of
          3 -> UpSideDown
          6 -> CCW90
          8 -> CW90
          _ -> Normal
        _ -> Normal

  onDecodePng bs = return image
    where
      image = case JP.decodeImage . toStrict $ bs of
        Right img -> JP.convertRGB8 img
        Left e -> error e

  onEncodeJpeg quality image
    | quality >= 0 && quality <= 100 = return $ JP.encodeJpegAtQuality qlt img
    | otherwise = return $ JP.encodeJpeg img
    where
      qlt = fromIntegral quality
      img = convertImage image

  onEncodePng image = return (JP.encodePng image)

  onRotateToNormal orientation img = do
    return rotated
    where
      rotated = case orientation of
        Normal -> img
        CW90 -> rotateLeft90 $ img
        UpSideDown -> rotate180 $ img
        CCW90 -> rotateRight90 $ img

  onResizeImage size image = do
    return resized
    where
      newSize = calculateNewSize (JP.imageWidth image, JP.imageHeight image) size
      resized = uncurry scaleBilinear newSize image

runJuicyPixels :: ImpScript a -> IO a
runJuicyPixels = interpret
