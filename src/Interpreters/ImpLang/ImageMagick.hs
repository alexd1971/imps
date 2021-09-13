{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Interpreters.ImpLang.ImageMagick
  ( run
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import DSL.ImpLang
  ( Image
  , ImpScript
  , Interpreter(..)
  , Orientation(..)
  , Quality(Default, Quality)
  , interpret
  )
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import GHC.IO.Handle.Internals (withHandle')
import qualified Graphics.HsExif as Exif
import Graphics.ImageMagick.MagickCore.Types.FFI.FilterTypes (undefinedFilter)
import Graphics.ImageMagick.MagickWand (PMagickWand, magickWand, shaveImage, resetImagePage, distortImage, undefinedDistortion)
import Graphics.ImageMagick.MagickWand.FFI.MagickWand
  ( destroyMagickWand
  , magickWandGenesis
  , magickWandTerminus
  , newMagickWand
  )
import Graphics.ImageMagick.MagickWand.WandImage
  ( cropImage
  , getImageBlob
  , getImageHeight
  , getImageWidth
  , readImageBlob
  , resizeImage
  , setImageCompressionQuality
  )
import Helpers.ResizeRules (containRule, coverRule)

type instance Image = PMagickWand

instance Interpreter (ResourceT IO) where
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
    magickWand <- liftIO newMagickWand
    readImageBlob magickWand (toStrict bs)
    return (magickWand, orientation)
  onDecodePng bs = do
    magickWand <- liftIO newMagickWand
    readImageBlob magickWand (toStrict bs)
    return magickWand
  onEncodeJpeg (Quality q) magickWand = do
    setImageCompressionQuality magickWand q
    encodeImage magickWand
  onEncodeJpeg Default magickWand = encodeImage magickWand
  onEncodePng magickWand = encodeImage magickWand
  onRotateToNormal _ magickWand = pure magickWand
  onResize (w, h) magickWand = do
    resizeImage magickWand w h undefinedFilter 1
    return magickWand
  onCrop (width, height) magickWand = do
    (w, h) <- onGetImageSize magickWand
    let w' = min width w
        h' = min height h
    if (w', h') == (w, h)
      then return magickWand
      else do
        let x = (w - w') `div` 2
            y = (h - h') `div` 2
        cropImage magickWand w' h' x y
        resetImagePage magickWand (Just "0x0")
        return magickWand
  onGetImageSize magickWand = do
    w <- getImageWidth magickWand
    h <- getImageHeight magickWand
    return (w, h)

encodeImage :: PMagickWand -> ResourceT IO ByteString
encodeImage magickWand = do
  bs <- getImageBlob magickWand
  liftIO . destroyMagickWand $ magickWand
  return . fromStrict $ bs

run :: ImpScript a -> IO a
run = runResourceT . interpret
