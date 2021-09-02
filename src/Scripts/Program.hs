{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripts.Program where

import DSL.ControlProgram (ControlProgram, eval, impScript, ioScript)
import DSL.IOLang
  ( ImageType(..)
  , Input
  , Output
  , OutputResult
  , reportError
  , writeImageBS
  )
import Scripts.ReadInput (ImageData(..), readInput)
import Scripts.Resize (imageJpegResize, imagePngResize)

program :: Input -> Output -> ControlProgram OutputResult
program input output = do
  ImageData {..} <- eval . ioScript . readInput $ input
  if size /= 0 && imageType /= Unsupported
    then do
      bs <-
        eval . impScript $
        case imageType of
          Jpeg -> imageJpegResize imageBS size quality
          Png -> imagePngResize imageBS size
          _ -> return ""
      eval . ioScript $ writeImageBS output bs
    else eval . ioScript $ reportError output "Not set size or image type"
