{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module DSL.IOLang
  ( IOScript
  , Interpreter(..)
  , Input
  , ImageType(..)
  , InputData(..)
  , Output
  , OutputResult
  , readInput
  , writeImageBS
  , reportError
  , interpret
  ) where

import Control.Monad.Free (Free(..))
import DSL.ImpLang (Height, Quality, ResizeRule, Width)
import Data.ByteString.Lazy (ByteString)
import Data.Text

type family Input

type family Output

type family OutputResult

data ImageType
  = Jpeg
  | Png
  deriving (Eq, Show)

data InputData =
  InputData
    { imageType :: Maybe ImageType
    , width :: Maybe Width
    , height :: Maybe Height
    , quality :: Quality
    , resizeRule :: Maybe ResizeRule
    , imageBS :: ByteString
    }

data IOLang next
  = ReadInput Input (Either Text InputData -> next)
  | WriteImageBS Output ByteString (OutputResult -> next)
  | ReportError Output Text (OutputResult -> next)
  deriving (Functor)

type IOScript = Free IOLang

readInput :: Input -> IOScript (Either Text InputData)
readInput input = Free $ ReadInput input Pure

writeImageBS :: Output -> ByteString -> IOScript OutputResult
writeImageBS output bs = Free $ WriteImageBS output bs Pure

reportError :: Output -> Text -> IOScript OutputResult
reportError output message = Free $ ReportError output message Pure

class Monad m =>
      Interpreter m
  where
  onReadInput :: Input -> m (Either Text InputData)
  onWriteImageBS :: Output -> ByteString -> m OutputResult
  onReportError :: Output -> Text -> m OutputResult

interpret ::
     Monad m
  => Interpreter m =>
       IOScript a -> m a
interpret (Pure a) = return a
interpret (Free (ReadInput input next)) = do
  bs <- onReadInput input
  interpret (next bs)
interpret (Free (WriteImageBS output bs next)) = do
  result <- onWriteImageBS output bs
  interpret (next result)
interpret (Free (ReportError output message next)) = do
  result <- onReportError output message
  interpret (next result)
