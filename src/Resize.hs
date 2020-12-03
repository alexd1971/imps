module Resize where

import           Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import           Graphics.GD          (Image, Size, imageSize,
                                       loadJpegByteString, loadPngByteString,
                                       resizeImage, saveJpegByteString,
                                       savePngByteString)

-- Converts raw jpeg image data to raw resized jpeg image data
imageJpegResize :: Size -> ByteString -> IO ByteString
imageJpegResize size imageBS = fromStrict <$> (decode imageBS >>= gdResize size >>= encode)
  where decode = loadJpegByteString . toStrict
        encode = saveJpegByteString (-1)

-- Converts raw png image data to raw resized png image data
imagePngResize :: Size -> ByteString -> IO ByteString
imagePngResize size imageBS = fromStrict <$> (decode imageBS >>= gdResize size >>= encode)
  where decode = loadPngByteString . toStrict
        encode = savePngByteString

-- Makes image resize with GD library
gdResize :: Size -> Image -> IO Image
gdResize requestedSize image = do
  actualSize <- imageSize image
  let newSize = calculateNewSize actualSize requestedSize
  uncurry resizeImage newSize image

-- Calculates new image size
calculateNewSize :: Size -> Size -> Size
calculateNewSize actualSize requestedSize
  | requestedWidth == 0 && requestedHeight == 0 = actualSize
  | requestedHeight == 0 = sizeByWidth
  | requestedWidth == 0 = sizeByHeight
  | widthRatio < heightRatio = sizeByWidth
  | otherwise = sizeByHeight
  where actualWidth = fst actualSize
        actualHeight = snd actualSize
        requestedWidth = fst requestedSize
        requestedHeight = snd requestedSize
        widthRatio = fromIntegral requestedWidth / fromIntegral actualWidth
        heightRatio = fromIntegral requestedHeight / fromIntegral actualHeight
        sizeByWidth = (requestedWidth, round (fromIntegral actualHeight * widthRatio))
        sizeByHeight = (round (fromIntegral actualWidth * heightRatio), requestedHeight)
