module Helpers where

-- Calculates new image size
calculateNewSize :: (Int, Int) -> Int -> (Int, Int)
calculateNewSize actualSize requestedSize
  | requestedWidth == 0 && requestedHeight == 0 = actualSize
  | requestedHeight == 0 = sizeByWidth
  | requestedWidth == 0 = sizeByHeight
  | widthRatio < heightRatio = sizeByWidth
  | otherwise = sizeByHeight
  where
    actualWidth = fst actualSize
    actualHeight = snd actualSize
    requestedWidth = requestedSize
    requestedHeight = requestedSize
    widthRatio = fromIntegral requestedWidth / fromIntegral actualWidth
    heightRatio = fromIntegral requestedHeight / fromIntegral actualHeight
    sizeByWidth = (requestedWidth, round (fromIntegral actualHeight * widthRatio))
    sizeByHeight = (round (fromIntegral actualWidth * heightRatio), requestedHeight)
