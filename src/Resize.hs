module Resize where

import DSL
  ( Imp,
    Quality,
    Size,
    decodeJpeg,
    decodePng,
    encodeJpeg,
    encodePng,
    resizeImage,
    rotateToNormal,
  )
import Data.ByteString.Lazy (ByteString)

imageJpegResize :: ByteString -> Size -> Quality -> Imp ByteString
imageJpegResize bs size quality = do
  (image, orientation) <- decodeJpeg bs
  rotateToNormal orientation image >>= resizeImage size >>= encodeJpeg quality

imagePngResize :: ByteString -> Size -> Imp ByteString
imagePngResize bs size = decodePng bs >>= resizeImage size >>= encodePng
