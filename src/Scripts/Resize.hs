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

jpegResizeToContain ::
     Maybe Width
  -> Maybe Height
  -> Quality
  -> ByteString
  -> ImpScript ByteString
jpegResizeToContain w h q bs = do
  (image, orientation) <- decodeJpeg bs
  imageSize <- getImageSize image
  let size =
        if orientation == Normal || orientation == UpSideDown
          then (w, h)
          else (h, w)
  size' <- uncurry containSize size imageSize
  resize size' image >>= encodeJpeg q

jpegResizeToCover ::
     Maybe Width
  -> Maybe Height
  -> Quality
  -> ByteString
  -> ImpScript ByteString
jpegResizeToCover w h q bs = do
  (image, orientation) <- decodeJpeg bs
  imageSize <- getImageSize image
  let size =
        if orientation == Normal || orientation == UpSideDown
          then (w, h)
          else (h, w)
  size' <- uncurry coverSize size imageSize
  resize size' image >>= crop (uncurry cropSize size size') >>= encodeJpeg q

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
  resize size image >>= crop (cropSize w h size) >>= encodePng

cropSize :: Maybe Width -> Maybe Height -> Size -> Size
cropSize w h (w', h') = (fromMaybe w' w, fromMaybe h' h)
