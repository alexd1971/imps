module Interpreters.JuicyPixels (runJuicyPixels) where

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
    Interpreter (onDecodeJpeg, onDecodePng, onEncodeJpeg, onEncodePng, onResizeImage, onRotateToNormal),
    Orientation (CCW90, CW180, CW90, Normal),
    interpret,
  )
import Data.ByteString.Lazy (toStrict)
import Data.Dynamic (fromDynamic, toDyn)
import Data.Functor.Identity
import Helpers (calculateNewSize)

instance Interpreter Identity where
  onDecodeJpeg bs = return (toDyn image, orientation)
    where
      (image, metadata) = case decodeImageWithMetadata . toStrict $ bs of
        Right (img, meta) -> (convertRGB8 img, meta)
        Left e -> error e
      orientation = case lookup TagOrientation . extractExifMetas $ metadata of
        Just (ExifShort v) -> case v of
          3 -> CW180
          6 -> CCW90
          8 -> CW90
          _ -> Normal
        _ -> Normal
  onDecodePng bs = return (toDyn image)
    where
      image = case decodeImage . toStrict $ bs of
        Right img -> convertRGB8 img
        Left e -> error e
  onEncodeJpeg quality img = return (encodeJpegAtQuality q i)
    where
      q = fromIntegral quality
      i = case fromDynamic img :: Maybe (Image PixelRGB8) of
        Just image -> convertImage image
        Nothing -> error "Cannot convert jpeg"
  onEncodePng img = return (encodePng i)
    where
      i = case fromDynamic img :: Maybe (Image PixelRGB8) of
        Just image -> image
        Nothing -> error "Cannot convert png"
  onRotateToNormal o img = return rotated
    where
      original = case fromDynamic img :: Maybe (Image PixelRGB8) of
        Just image -> image
        Nothing -> error "Cannot get image"
      rotated = case o of
        Normal -> img
        CW90 -> toDyn . rotateLeft90 $ original
        CW180 -> toDyn . rotate180 $ original
        CCW90 -> toDyn . rotateRight90 $ original
  onResizeImage s img = return resized
    where
      original = case fromDynamic img :: Maybe (Image PixelRGB8) of
        Just image -> image
        Nothing -> error "Cannot get image"
      size = calculateNewSize (imageWidth original, imageHeight original) s
      resized = toDyn $ uncurry scaleBilinear size original

runJuicyPixels :: Imp a -> IO a
runJuicyPixels = return . runIdentity . interpret
