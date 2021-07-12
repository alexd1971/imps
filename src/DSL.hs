{-# LANGUAGE GADTs #-}

module DSL
  ( Img,
    Size,
    Quality,
    Orientation (..),
    Imp,
    toDyn,
    fromDynamic,
    decodeJpeg,
    encodeJpeg,
    decodePng,
    encodePng,
    rotateToNormal,
    resizeImage,
    Interpreter (..),
    interpret,
  )
where

import Control.Monad.Free
import Data.ByteString.Lazy
import Data.Dynamic

type Img = Dynamic

type Size = Int

type Quality = Int

data Orientation = Normal | CW90 | CCW90 | CW180 deriving (Show)

data ImpF next
  = DecodeJpeg ByteString ((Img, Orientation) -> next)
  | DecodePng ByteString (Img -> next)
  | EncodeJpeg Quality Img (ByteString -> next)
  | EncodePng Img (ByteString -> next)
  | RotateToNormal Orientation Img (Img -> next)
  | ResizeImage Size Img (Img -> next)

type Imp = Free ImpF

instance Functor ImpF where
  fmap f (DecodeJpeg bs g) = DecodeJpeg bs (f . g)
  fmap f (RotateToNormal o img g) = RotateToNormal o img (f . g)
  fmap f (DecodePng bs g) = DecodePng bs (f . g)
  fmap f (ResizeImage s img g) = ResizeImage s img (f . g)
  fmap f (EncodeJpeg q img g) = EncodeJpeg q img (f . g)
  fmap f (EncodePng img g) = EncodePng img (f . g)

decodeJpeg :: ByteString -> Imp (Img, Orientation)
decodeJpeg bs = liftF $ DecodeJpeg bs id

encodeJpeg :: Quality -> Img -> Imp ByteString
encodeJpeg quality img = liftF $ EncodeJpeg quality img id

decodePng :: ByteString -> Imp Img
decodePng bs = liftF $ DecodePng bs id

encodePng :: Img -> Imp ByteString
encodePng img = liftF $ EncodePng img id

rotateToNormal :: Orientation -> Img -> Imp Img
rotateToNormal orientation img = liftF $ RotateToNormal orientation img id

resizeImage :: Size -> Img -> Imp Img
resizeImage size img = liftF $ ResizeImage size img id

class Monad m => Interpreter m where
  onDecodeJpeg :: ByteString -> m (Img, Orientation)
  onDecodePng :: ByteString -> m Img
  onEncodeJpeg :: Quality -> Img -> m ByteString
  onEncodePng :: Img -> m ByteString
  onRotateToNormal :: Orientation -> Img -> m Img
  onResizeImage :: Size -> Img -> m Img

interpret :: Monad m => Interpreter m => Imp a -> m a
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
