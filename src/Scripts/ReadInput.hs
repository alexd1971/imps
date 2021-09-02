{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scripts.ReadInput where

import DSL.IOLang
  ( IOScript
  , ImageType
  , Input
  , getImageType
  , getQuality
  , getSize
  , readImageBS
  )
import DSL.ImpLang (Quality, Size)
import Data.ByteString.Lazy (ByteString)

data ImageData =
  ImageData
    { imageType :: ImageType
    , size :: Size
    , quality :: Quality
    , imageBS :: ByteString
    }

readInput :: Input -> IOScript ImageData
readInput input = do
  size <- getSize input
  imageType <- getImageType input
  quality <- getQuality input
  imageBS <- readImageBS input
  return ImageData {..}
