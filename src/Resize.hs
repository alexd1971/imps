module Resize where

import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import Graphics.GD
  ( Image,
    Size,
    imageSize,
    loadJpegByteString,
    loadPngByteString,
    resizeImage,
    rotateImage,
    saveJpegByteString,
    savePngByteString,
  )
import Graphics.HsExif
  ( ImageOrientation (Rotation),
    RotationDirection (HundredAndEighty, MinusNinety, Ninety),
    getOrientation,
    parseExif,
  )

-- Tries to get image orientation
imageOrientation :: ByteString -> Maybe ImageOrientation
imageOrientation imageBS =
  let eitherExif = parseExif imageBS
   in case eitherExif of
        Left _ -> Nothing
        Right exif -> getOrientation exif

-- Rotates image to normal orientation
rotateToNormal :: Maybe ImageOrientation -> Image -> IO Image
rotateToNormal orientation image = do
  case orientation of
    Just (Rotation Ninety) -> rotateImage 1 image
    Just (Rotation HundredAndEighty) -> rotateImage 2 image
    Just (Rotation MinusNinety) -> rotateImage 3 image
    _ -> return image

-- Converts raw jpeg image data to raw resized jpeg image data
imageJpegResize :: Int -> Size -> ByteString -> IO ByteString
imageJpegResize quality size imageBS
  | quality >= (-1) && quality <= 95 = fromStrict <$> (decode >>= resize >>= rotate >>= encode)
  | otherwise = imageJpegResize (-1) size imageBS
  where
    decode = loadJpegByteString . toStrict $ imageBS
    resize = gdResize size
    rotate = rotateToNormal $ imageOrientation imageBS
    encode = saveJpegByteString quality

-- Converts raw png image data to raw resized png image data
imagePngResize :: Size -> ByteString -> IO ByteString
imagePngResize size imageBS = fromStrict <$> (decode imageBS >>= gdResize size >>= encode)
  where
    decode = loadPngByteString . toStrict
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
  where
    actualWidth = fst actualSize
    actualHeight = snd actualSize
    requestedWidth = fst requestedSize
    requestedHeight = snd requestedSize
    widthRatio = fromIntegral requestedWidth / fromIntegral actualWidth
    heightRatio = fromIntegral requestedHeight / fromIntegral actualHeight
    sizeByWidth = (requestedWidth, round (fromIntegral actualHeight * widthRatio))
    sizeByHeight = (round (fromIntegral actualWidth * heightRatio), requestedHeight)
