{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module DSL.IOLang
  ( IOScript
  , Interpreter(..)
  , Input
  , ImageType(..)
  , Output
  , OutputResult
  , readImageBS
  , writeImageBS
  , getSize
  , getQuality
  , getImageType
  , reportError
  , interpret
  ) where

import Control.Monad.Free (Free(..))
import DSL.ImpLang (Quality, Size)
import Data.ByteString.Lazy (ByteString)

type family Input

type family Output

type family OutputResult

data ImageType
  = Unsupported
  | Jpeg
  | Png
  deriving (Eq)

data IOLang next
  = ReadImageBS Input (ByteString -> next)
  | WriteImageBS Output ByteString (OutputResult -> next)
  | GetSize Input (Size -> next)
  | GetQuality Input (Quality -> next)
  | GetImageType Input (ImageType -> next)
  | ReportError Output String (OutputResult -> next)
  deriving (Functor)

type IOScript = Free IOLang

readImageBS :: Input -> IOScript ByteString
readImageBS input = Free $ ReadImageBS input Pure

writeImageBS :: Output -> ByteString -> IOScript OutputResult
writeImageBS output bs = Free $ WriteImageBS output bs Pure

getSize :: Input -> IOScript Size
getSize input = Free $ GetSize input Pure

getQuality :: Input -> IOScript Quality
getQuality input = Free $ GetQuality input Pure

getImageType :: Input -> IOScript ImageType
getImageType input = Free $ GetImageType input Pure

reportError :: Output -> String -> IOScript OutputResult
reportError output message = Free $ ReportError output message Pure

class Monad m =>
      Interpreter m
  where
  onReadImageBS :: Input -> m ByteString
  onWriteImageBS :: Output -> ByteString -> m OutputResult
  onGetSize :: Input -> m Size
  onGetQuality :: Input -> m Quality
  onGetImageType :: Input -> m ImageType
  onReportError :: Output -> String -> m OutputResult

interpret ::
     Monad m
  => Interpreter m =>
       IOScript a -> m a
interpret (Pure a) = return a
interpret (Free (ReadImageBS input next)) = do
  bs <- onReadImageBS input
  interpret (next bs)
interpret (Free (WriteImageBS output bs next)) = do
  result <- onWriteImageBS output bs
  interpret (next result)
interpret (Free (GetSize input next)) = do
  size <- onGetSize input
  interpret (next size)
interpret (Free (GetQuality input next)) = do
  quality <- onGetQuality input
  interpret (next quality)
interpret (Free (GetImageType input next)) = do
  imageType <- onGetImageType input
  interpret (next imageType)
interpret (Free (ReportError output message next)) = do
  result <- onReportError output message
  interpret (next result)
