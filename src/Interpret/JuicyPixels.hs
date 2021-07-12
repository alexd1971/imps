module Interpret.JuicyPixels where

import Codec.Picture
  ( Image (imageHeight, imageWidth),
    PixelRGB8,
    convertRGB8,
    decodeImage,
    decodeImageWithMetadata,
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
  ( Imp,
    ImpF (DecodeJpeg, DecodePng, EncodeJpeg, EncodePng, ResizeImage, RotateToNormal),
    Orientation (CCW90, CW180, CW90, Normal),
  )
import Data.ByteString.Lazy (toStrict)
import Data.Dynamic (fromDynamic, toDyn)
import Helpers (calculateNewSize)

runJuicyPixels :: Imp a -> a
runJuicyPixels (Pure a) = a
runJuicyPixels (Free (DecodeJpeg bs f)) = runJuicyPixels . f $ (toDyn image, orientation)
  where
    (image, metadata) = case decodeImageWithMetadata . toStrict $ bs of
      Right (img, meta) -> (convertRGB8 img, meta)
      Left e -> error e
    orientation = case lookup TagOrientation . extractExifMetas $ metadata of
      Just (ExifShort v) -> case v of
        1 -> Normal
        2 -> Normal
        3 -> CW180
        4 -> Normal
        5 -> Normal
        6 -> CCW90
        7 -> Normal
        8 -> CW90
      Nothing -> Normal
runJuicyPixels (Free (DecodePng bs f)) = runJuicyPixels . f $ toDyn image
  where
    image = case decodeImage . toStrict $ bs of
      Right img -> convertRGB8 img
      Left e -> error e
runJuicyPixels (Free (EncodeJpeg quality img g)) = runJuicyPixels . g $ encodeJpegAtQuality q i
  where
    q = fromIntegral quality
    i = case fromDynamic img :: Maybe (Image PixelRGB8) of
      Just image -> convertImage image
      Nothing -> error "Cannot convert jpeg"
runJuicyPixels (Free (EncodePng img g)) = runJuicyPixels . g . encodePng $ i
  where
    i = case fromDynamic img :: Maybe (Image PixelRGB8) of
      Just image -> image
      Nothing -> error "Cannot convert png"
runJuicyPixels (Free (RotateToNormal o img g)) = runJuicyPixels . g $ rotated
  where
    original = case fromDynamic img :: Maybe (Image PixelRGB8) of
      Just image -> image
      Nothing -> error "Cannot get image"
    rotated = case o of
      Normal -> img
      CW90 -> toDyn . rotateLeft90 $ original
      CW180 -> toDyn . rotate180 $ original
      CCW90 -> toDyn . rotateRight90 $ original
runJuicyPixels (Free (ResizeImage s img g)) = runJuicyPixels . g $ resized
  where
    original = case fromDynamic img :: Maybe (Image PixelRGB8) of
      Just image -> image
      Nothing -> error "Cannot get image"
    size = calculateNewSize (imageWidth original, imageHeight original) s
    resized = toDyn $ uncurry scaleBilinear size original
