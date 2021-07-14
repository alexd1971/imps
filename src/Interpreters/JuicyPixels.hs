{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  ( Img (..),
    Imp,
    Interpreter (onDecodeJpeg, onDecodePng, onEncodeJpeg, onEncodePng, onResizeImage, onRotateToNormal),
    Orientation (CCW90, CW180, CW90, Normal),
    interpret,
  )
import Data.ByteString.Lazy (toStrict)
import Data.Dynamic (fromDynamic, toDyn)
import Data.Functor.Identity
import Helpers (calculateNewSize)

instance Interpreter (Image PixelRGB8) Identity where
  onDecodeJpeg bs = return (Img image, orientation)
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

  onDecodePng bs = return (Img image)
    where
      image = case decodeImage . toStrict $ bs of
        Right img -> convertRGB8 img
        Left e -> error e

  onEncodeJpeg quality (Img jpImage) = return (encodeJpegAtQuality quality image)
    where
      quality = fromIntegral quality
      image = convertImage jpImage

  onEncodePng (Img jpImage) = return (encodePng jpImage)

  onRotateToNormal orientation image@(Img jpImage) = return rotated
    where
      rotated = case orientation of
        Normal -> image
        CW90 -> Img . rotateLeft90 $ jpImage
        CW180 -> Img . rotate180 $ jpImage
        CCW90 -> Img . rotateRight90 $ jpImage

  onResizeImage size (Img jpImage) = return resized
    where
      newSize = calculateNewSize (imageWidth jpImage, imageHeight jpImage) size
      resized = Img $ uncurry scaleBilinear newSize jpImage

runJuicyPixels :: Imp (Image PixelRGB8) a -> IO a
runJuicyPixels = return . runIdentity . interpret
