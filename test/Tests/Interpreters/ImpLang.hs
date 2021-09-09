{-# LANGUAGE TypeFamilies #-}

module Tests.Interpreters.ImpLang where

import DSL.ImpLang
  ( Image
  , ImpScript
  , Interpreter(..)
  , Orientation(..)
  , Quality(..)
  , interpret
  )
import Data.ByteString.Lazy
import Data.Functor.Identity
import Helpers.ResizeRules
import MockImageLibrary

type instance Image = MockImage

instance Interpreter Identity where
  onDecodeJpeg bs = do
    let image = decode . toStrict $ bs
    return (image, orientation image)
  onDecodePng bs = return . decode . toStrict $ bs
  onEncodeJpeg qlty image = return . fromStrict . encode $ image {quality = qlty}
  onEncodePng image =
    return . fromStrict . encode $
    image {orientation = Normal, quality = Quality 100}
  onRotateToNormal orientation image = do
    let rotated =
          case orientation of
            Normal -> image
            CW90 -> rotate CCW image
            UpSideDown -> rotate CW . rotate CW $ image
            CCW90 -> rotate CW image
    return rotated
  onResize size image = do
    return $ resize size image
  onCrop size image = return $ crop size image
  onGetImageSize image = return $ (width image, height image)
  onContainSize w h s = return $ containRule w h s
  onCoverSize w h s = return $ coverRule w h s

run :: ImpScript a -> a
run = runIdentity . interpret
