{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scripts.ReadData where

import DSL.IOLang
  ( IOScript
  , ImageType
  , Input
  , InputData
  , Output
  , OutputResult
  , readInput
  , reportError
  , reportError
  )
import DSL.ImpLang (Height, Quality, ResizeRule, Width)
import Data.ByteString.Lazy (ByteString)

readData :: Input -> Output -> IOScript (Either OutputResult InputData)
readData input output = do
  readResult <- readInput input
  case readResult of
    Right inputData -> return $ Right inputData
    Left error -> Left <$> reportError output error
