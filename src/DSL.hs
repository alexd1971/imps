{-# LANGUAGE GADTs #-}

module DSL where

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

