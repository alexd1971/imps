module Helpers.ResizeRules
  ( containRule
  , coverRule
  , ratio
  ) where

import DSL.ImpLang (Height, ResizeRule, Size, Width)

containRule :: Maybe Width -> Maybe Height -> Size -> Size
containRule Nothing Nothing size' = size'
containRule Nothing (Just h) size' = sizeByHeight h size'
containRule (Just w) Nothing size' = sizeByWidth w size'
containRule (Just w) (Just h) size'@(w', h')
  | r > r' = sizeByWidth w size'
  | otherwise = sizeByHeight h size'
  where
    r = ratio h w
    r' = ratio h' w'

coverRule :: Maybe Width -> Maybe Height -> Size -> Size
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

sizeByWidth :: Width -> Size -> Size
sizeByWidth w (w', h') = (w, round (fromIntegral w * ratio h' w'))

sizeByHeight :: Height -> Size -> Size
sizeByHeight h (w', h') = (round (fromIntegral h * ratio w' h'), h)
