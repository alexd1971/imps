module Helpers.ResizeRules
  ( containRule
  , coverRule
  , ratio
  ) where

containRule :: Maybe Int -> Maybe Int -> (Int, Int) -> (Int, Int)
containRule Nothing Nothing size' = size'
containRule Nothing (Just h) size' = sizeByHeight h size'
containRule (Just w) Nothing size' = sizeByWidth w size'
containRule (Just w) (Just h) size'@(w', h')
  | r > r' = sizeByWidth w size'
  | otherwise = sizeByHeight h size'
  where
    r = ratio h w
    r' = ratio h' w'

coverRule :: Maybe Int -> Maybe Int -> (Int, Int) -> (Int, Int)
coverRule Nothing Nothing size' = size'
coverRule Nothing (Just h) size' = sizeByHeight h size'
coverRule (Just w) Nothing size' = sizeByWidth w size'
coverRule (Just w) (Just h) size'@(w', h')
  | r < r' = sizeByWidth w size'
  | otherwise = sizeByHeight h size'
  where
    r = ratio h w
    r' = ratio h' w'

ratio :: Integral a => a -> a -> Double
ratio x y = fromIntegral x / fromIntegral y

sizeByWidth :: Int -> (Int, Int) -> (Int, Int)
sizeByWidth w (w', h') = (w, round (fromIntegral w * ratio h' w'))

sizeByHeight :: Int -> (Int, Int) -> (Int, Int)
sizeByHeight h (w', h') = (round (fromIntegral h * ratio w' h'), h)
