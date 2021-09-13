{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module DSL.ImpLang
  ( Image
  , Size
  , Quality(..)
  , Orientation(..)
  , ResizeRule(..)
  , ImpScript
  , Width
  , Height
  , Interpreter(..)
  , crop
  , decodeJpeg
  , decodePng
  , encodeJpeg
  , encodePng
  , resize
  , getImageSize
  , containSize
  , coverSize
  , rotateToNormal
  , interpret
  ) where

import Control.Monad.Free
import Data.ByteString.Lazy
import Helpers.ResizeRules

type family Image

type Width = Int

type Height = Int

type Size = (Width, Height)

data Quality
  = Default
  | Quality Int
  deriving (Eq, Show, Read)

data Orientation
  = Normal
  | CW90
  | UpSideDown
  | CCW90
  deriving (Eq, Show, Read, Enum, Bounded)

data ResizeRule
  = Contain
  | Cover
  deriving (Eq, Show, Read, Enum, Bounded)

data ImpLang next
  = DecodeJpeg ByteString ((Image, Orientation) -> next)
  | DecodePng ByteString (Image -> next)
  | EncodeJpeg Quality Image (ByteString -> next)
  | EncodePng Image (ByteString -> next)
  | RotateToNormal Orientation Image (Image -> next)
  | GetImageSize Image (Size -> next)
  | ContainSize (Maybe Width) (Maybe Height) Size (Size -> next)
  | CoverSize (Maybe Width) (Maybe Height) Size (Size -> next)
  | Resize Size Image (Image -> next)
  | Crop Size Image (Image -> next)
  deriving (Functor)

type ImpScript = Free ImpLang

decodeJpeg :: ByteString -> ImpScript (Image, Orientation)
decodeJpeg bs = liftF $ DecodeJpeg bs id

encodeJpeg :: Quality -> Image -> ImpScript ByteString
encodeJpeg quality image = liftF $ EncodeJpeg quality image id

decodePng :: ByteString -> ImpScript Image
decodePng bs = liftF $ DecodePng bs id

encodePng :: Image -> ImpScript ByteString
encodePng image = liftF $ EncodePng image id

rotateToNormal :: Orientation -> Image -> ImpScript Image
rotateToNormal orientation image = liftF $ RotateToNormal orientation image id

getImageSize :: Image -> ImpScript Size
getImageSize image = liftF $ GetImageSize image id

containSize :: Maybe Width -> Maybe Height -> Size -> ImpScript Size
containSize w h size = liftF $ ContainSize w h size id

coverSize :: Maybe Width -> Maybe Height -> Size -> ImpScript Size
coverSize w h size = liftF $ CoverSize w h size id

resize :: Size -> Image -> ImpScript Image
resize size image = liftF $ Resize size image id

crop :: Size -> Image -> ImpScript Image
crop size image = liftF $ Crop size image id

class Monad m =>
      Interpreter m
  where
  onDecodeJpeg :: ByteString -> m (Image, Orientation)
  onDecodePng :: ByteString -> m Image
  onEncodeJpeg :: Quality -> Image -> m ByteString
  onEncodePng :: Image -> m ByteString
  onRotateToNormal :: Orientation -> Image -> m Image
  onContainSize :: Maybe Width -> Maybe Height -> Size -> m Size
  onContainSize w h s = pure $ containRule w h s
  onCoverSize :: Maybe Width -> Maybe Height -> Size -> m Size
  onCoverSize w h s = pure $ coverRule w h s
  onResize :: Size -> Image -> m Image
  onCrop :: Size -> Image -> m Image
  onGetImageSize :: Image -> m Size

interpret ::
     Monad m
  => Interpreter m =>
       ImpScript a -> m a
interpret (Pure a) = return a
interpret (Free (DecodeJpeg bs next)) = do
  image <- onDecodeJpeg bs
  interpret (next image)
interpret (Free (DecodePng bs next)) = do
  image <- onDecodePng bs
  interpret (next image)
interpret (Free (EncodeJpeg quality image next)) = do
  bs <- onEncodeJpeg quality image
  interpret (next bs)
interpret (Free (EncodePng image next)) = do
  bs <- onEncodePng image
  interpret (next bs)
interpret (Free (RotateToNormal orientation image next)) = do
  image <- onRotateToNormal orientation image
  interpret (next image)
interpret (Free (Resize size image next)) = do
  image <- onResize size image
  interpret (next image)
interpret (Free (Crop size image next)) = do
  image <- onCrop size image
  interpret (next image)
interpret (Free (ContainSize width height size next)) = do
  size' <- onContainSize width height size
  interpret (next size')
interpret (Free (CoverSize width height size next)) = do
  size' <- onCoverSize width height size
  interpret (next size')
interpret (Free (GetImageSize image next)) = do
  size <- onGetImageSize image
  interpret (next size)
