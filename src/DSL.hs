{-# LANGUAGE MultiParamTypeClasses #-}

module DSL
  ( Img (..),
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

newtype Img a = Img a

type Size = Int

type Quality = Int

data Orientation = Normal | CW90 | CCW90 | CW180 deriving (Show)

data ImpF a next
  = DecodeJpeg ByteString ((Img a, Orientation) -> next)
  | DecodePng ByteString (Img a -> next)
  | EncodeJpeg Quality (Img a) (ByteString -> next)
  | EncodePng (Img a) (ByteString -> next)
  | RotateToNormal Orientation (Img a) (Img a -> next)
  | ResizeImage Size (Img a) (Img a -> next)

instance Functor (ImpF a) where
  fmap f (DecodeJpeg bs g) = DecodeJpeg bs (f . g)
  fmap f (RotateToNormal o img g) = RotateToNormal o img (f . g)
  fmap f (DecodePng bs g) = DecodePng bs (f . g)
  fmap f (ResizeImage s img g) = ResizeImage s img (f . g)
  fmap f (EncodeJpeg q img g) = EncodeJpeg q img (f . g)
  fmap f (EncodePng img g) = EncodePng img (f . g)

type Imp a = Free (ImpF a)

decodeJpeg :: ByteString -> Imp a (Img a, Orientation)
decodeJpeg bs = liftF $ DecodeJpeg bs id

encodeJpeg :: Quality -> Img a -> Imp a ByteString
encodeJpeg quality img = liftF $ EncodeJpeg quality img id

decodePng :: ByteString -> Imp a (Img a)
decodePng bs = liftF $ DecodePng bs id

encodePng :: Img a -> Imp a ByteString
encodePng img = liftF $ EncodePng img id

rotateToNormal :: Orientation -> Img a -> Imp a (Img a)
rotateToNormal orientation img = liftF $ RotateToNormal orientation img id

resizeImage :: Size -> Img a -> Imp a (Img a)
resizeImage size img = liftF $ ResizeImage size img id

class Monad m => Interpreter a m where
  onDecodeJpeg :: ByteString -> m (Img a, Orientation)
  onDecodePng :: ByteString -> m (Img a)
  onEncodeJpeg :: Quality -> Img a -> m ByteString
  onEncodePng :: Img a -> m ByteString
  onRotateToNormal :: Orientation -> Img a -> m (Img a)
  onResizeImage :: Size -> Img a -> m (Img a)

interpret :: Monad m => Interpreter b m => Imp b a -> m a
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
