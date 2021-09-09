{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.Program where

import Control.Monad.Writer
import DSL.ControlProgram (ControlProgram, eval, impScript, ioScript)
import DSL.IOLang
  ( ImageType(..)
  , Input
  , InputData(..)
  , Output
  , OutputResult
  , reportError
  , writeImageBS
  )
import DSL.ImpLang
import Data.Text
import Scripts.ReadData
import Scripts.Resize

program :: Input -> Output -> ControlProgram OutputResult
program input output = do
  readInputResult <- eval . ioScript $ readData input output
  case readInputResult of
    Left r -> return r
    Right InputData {..} -> do
      bs <-
        eval . impScript $
        case imageType of
          Just Jpeg ->
            case resizeRule of
              Just Cover -> jpegResizeToCover width height quality imageBS
              _ -> jpegResizeToContain width height quality imageBS
          _ ->
            case resizeRule of
              Just Cover -> pngResizeToCover width height imageBS
              _ -> pngResizeToContain width height imageBS
      eval . ioScript $ writeImageBS output bs
