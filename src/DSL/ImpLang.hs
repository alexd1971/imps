{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module DSL.ImpLang
  ( Image
  , Size
  , Quality
  , Orientation(..)
  , ImpScript
  , decodeJpeg
  , encodeJpeg
  , decodePng
  , encodePng
  , rotateToNormal
  , resize
  , Interpreter(..)
  , interpret
  ) where

import Control.Monad.Free
import Data.ByteString.Lazy

type family Image

type Size = Int

type Quality = Int

data Orientation
  = Normal
  | CW90
  | UpSideDown
  | CCW90
  deriving (Eq, Show, Read, Enum, Bounded)

data ImpLang next
  = DecodeJpeg ByteString ((Image, Orientation) -> next)
  | DecodePng ByteString (Image -> next)
  | EncodeJpeg Quality Image (ByteString -> next)
  | EncodePng Image (ByteString -> next)
  | RotateToNormal Orientation Image (Image -> next)
  | ResizeImage Size Image (Image -> next)
  deriving (Functor)

type ImpScript = Free ImpLang

decodeJpeg :: ByteString -> ImpScript (Image, Orientation)
decodeJpeg bs = liftF $ DecodeJpeg bs id

encodeJpeg :: Quality -> Image -> ImpScript ByteString
encodeJpeg quality img = liftF $ EncodeJpeg quality img id

decodePng :: ByteString -> ImpScript Image
decodePng bs = liftF $ DecodePng bs id

encodePng :: Image -> ImpScript ByteString
encodePng img = liftF $ EncodePng img id

rotateToNormal :: Orientation -> Image -> ImpScript Image
rotateToNormal orientation img = liftF $ RotateToNormal orientation img id

resize :: Size -> Image -> ImpScript Image
resize size img = liftF $ ResizeImage size img id

class Monad m =>
      Interpreter m
  where
  onDecodeJpeg :: ByteString -> m (Image, Orientation)
  onDecodePng :: ByteString -> m Image
  onEncodeJpeg :: Quality -> Image -> m ByteString
  onEncodePng :: Image -> m ByteString
  onRotateToNormal :: Orientation -> Image -> m Image
  onResizeImage :: Size -> Image -> m Image

interpret ::
     Monad m
  => Interpreter m =>
       ImpScript a -> m a
interpret (Pure a) = return a
interpret (Free (DecodeJpeg bs next)) = do
  img <- onDecodeJpeg bs
  interpret (next img)
interpret (Free (DecodePng bs next)) = do
  img <- onDecodePng bs
  interpret (next img)
interpret (Free (EncodeJpeg q img next)) = do
  bs <- onEncodeJpeg q img
  interpret (next bs)
interpret (Free (EncodePng img next)) = do
  bs <- onEncodePng img
  interpret (next bs)
interpret (Free (RotateToNormal o img next)) = do
  img <- onRotateToNormal o img
  interpret (next img)
interpret (Free (ResizeImage s img next)) = do
  img <- onResizeImage s img
  interpret (next img)
