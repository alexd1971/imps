module Scripts.Resize where

import DSL.ImpLang
  ( Height
  , Image
  , ImpScript
  , Orientation(..)
  , Quality
  , ResizeRule(..)
  , Size
  , Width
  , containSize
  , coverSize
  , crop
  , decodeJpeg
  , decodePng
  , encodeJpeg
  , encodePng
  , getImageSize
  , resize
  , rotateToNormal
  )
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe, isNothing)
import Data.Tuple (swap)
import Network.HTTP.Types (status429)

imageResize :: Size -> Orientation -> Image -> ImpScript Image
imageResize size orientation image = do
  if orientation == Normal || orientation == UpSideDown
    then resize size image
    else resize (swap size) image

jpegResizeToContain ::
     Maybe Width
  -> Maybe Height
  -> Quality
  -> ByteString
  -> ImpScript ByteString
jpegResizeToContain w h q bs = do
  (image, orientation) <- decodeJpeg bs
  imageSize <- getImageSize image
  size <-
    if orientation == Normal || orientation == UpSideDown
      then containSize w h imageSize
      else containSize w h (swap imageSize)
  imageResize size orientation image >>= rotateToNormal orientation >>=
    encodeJpeg q

jpegResizeToCover ::
     Maybe Width
  -> Maybe Height
  -> Quality
  -> ByteString
  -> ImpScript ByteString
jpegResizeToCover w h q bs = do
  (image, orientation) <- decodeJpeg bs
  imageSize <- getImageSize image
  size <-
    if orientation == Normal || orientation == UpSideDown
      then coverSize w h imageSize
      else coverSize w h (swap imageSize)
  image' <- imageResize size orientation image >>= rotateToNormal orientation
  imageSize' <- getImageSize image'
  crop (cropSize w h imageSize') image' >>= encodeJpeg q

pngResizeToContain ::
     Maybe Width -> Maybe Height -> ByteString -> ImpScript ByteString
pngResizeToContain w h bs = do
  image <- decodePng bs
  imageSize <- getImageSize image
  size <- containSize w h imageSize
  resize size image >>= encodePng

pngResizeToCover ::
     Maybe Width -> Maybe Height -> ByteString -> ImpScript ByteString
pngResizeToCover w h bs = do
  image <- decodePng bs
  imageSize <- getImageSize image
  size <- coverSize w h imageSize
  image' <- resize size image
  imageSize' <- getImageSize image'
  crop (cropSize w h imageSize') image' >>= encodePng

cropSize :: Maybe Width -> Maybe Height -> Size -> Size
cropSize w h (w', h') = (fromMaybe w' w, fromMaybe h' h)
