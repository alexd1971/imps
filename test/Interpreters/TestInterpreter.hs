{-# LANGUAGE TypeFamilies #-}

module Interpreters.TestInterpreter where

import DSL (Image, Interpreter (..), Orientation (..), interpret, ImpScript)
import Data.ByteString.Lazy
import Data.Functor.Identity
import Helpers
import MockImageLibrary

type instance Image = MockImage

instance Interpreter Identity where
  onDecodeJpeg bs = do
    let image = decode . toStrict $ bs
    return (image, orientation image)

  onDecodePng bs = return . decode . toStrict $ bs

  onEncodeJpeg qlty image = return . fromStrict . encode $ image {quality = qlty}

  onEncodePng image = return . fromStrict . encode $ image {orientation = Normal, quality = 100}

  onRotateToNormal orientation image = do
    let rotated = case orientation of
          Normal -> image
          CW90 -> rotate CCW image
          UpSideDown -> rotate CW . rotate CW $ image
          CCW90 -> rotate CW image
    return rotated

  onResizeImage size image = do
    let newSize = calculateNewSize (width image, height image) size
        newWidth = fst newSize
        newHeight = snd newSize
    return $ resize newWidth newHeight image

run :: ImpScript a -> a
run = runIdentity . interpret
