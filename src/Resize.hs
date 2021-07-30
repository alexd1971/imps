module Resize where

import DSL
  ( ImpScript,
    Quality,
    Size,
    decodeJpeg,
    decodePng,
    encodeJpeg,
    encodePng,
    resize,
    rotateToNormal,
  )
import Data.ByteString.Lazy (ByteString)

imageJpegResize :: ByteString -> Size -> Quality -> ImpScript ByteString
imageJpegResize bs size quality = do
  (image, orientation) <- decodeJpeg bs
  resize size image >>= rotateToNormal orientation >>= encodeJpeg quality

imagePngResize :: ByteString -> Size -> ImpScript ByteString
imagePngResize bs size = decodePng bs >>= resize size >>= encodePng
