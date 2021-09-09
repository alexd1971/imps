{-# LANGUAGE TypeFamilies #-}

module Interpreters.ImpLang.GD where

import DSL.ImpLang
  ( Image
  , ImpScript
  , Interpreter(..)
  , Orientation(..)
  , Quality(..)
  , Size
  , interpret
  )
import Data.Maybe (fromMaybe)
import qualified Graphics.GD.ByteString.Lazy as GD
  ( Image
  , copyRegion
  , imageSize
  , loadJpegByteString
  , loadPngByteString
  , newImage
  , resizeImage
  , rotateImage
  , saveJpegByteString
  , savePngByteString
  )
import qualified Graphics.HsExif as Exif
  ( ImageOrientation(Normal, Rotation)
  , RotationDirection(HundredAndEighty, MinusNinety, Ninety)
  , getOrientation
  , parseExif
  )
import Helpers.ResizeRules

type instance Image = GD.Image

instance Interpreter IO where
  onDecodeJpeg bs = do
    let eitherExif = Exif.parseExif bs
        orientation =
          case eitherExif of
            Left _ -> Normal
            Right exif ->
              let maybeOrientation = Exif.getOrientation exif
               in case maybeOrientation of
                    Just (Exif.Rotation Exif.Ninety) -> CW90
                    Just (Exif.Rotation Exif.MinusNinety) -> CCW90
                    Just (Exif.Rotation Exif.HundredAndEighty) -> UpSideDown
                    _ -> Normal
    image <- GD.loadJpegByteString bs
    return (image, orientation)
  onDecodePng bs = GD.loadPngByteString bs
  onEncodeJpeg quality image = do
    case quality of
      Default -> GD.saveJpegByteString (-1) image
      Quality qlty -> GD.saveJpegByteString qlty image
  onEncodePng image = GD.savePngByteString image
  onRotateToNormal orientation image = do
    case orientation of
      CW90 -> GD.rotateImage 1 image
      CCW90 -> GD.rotateImage 3 image
      UpSideDown -> GD.rotateImage 2 image
      _ -> return image
  onResize size image = uncurry GD.resizeImage size image
  onCrop (width, height) image = do
    size@(w, h) <- GD.imageSize image
    let size' = (min width w, min height h)
    if size' == size
      then return image
      else do
        image' <- GD.newImage size'
        let point = ((w - fst size') `div` 2, (h - snd size') `div` 2)
        GD.copyRegion point size' image (0, 0) image'
        return image'
  onGetImageSize = GD.imageSize
  onContainSize w h s = pure $ containRule w h s
  onCoverSize w h s = pure $ coverRule w h s
