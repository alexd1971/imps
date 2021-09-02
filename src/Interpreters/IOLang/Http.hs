{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreters.IOLang.Http where

import Control.Monad (join)
import DSL.IOLang
  ( ImageType(Jpeg, Png, Unsupported)
  , Input
  , Interpreter(..)
  , Output
  , OutputResult
  )
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy.Char8 (pack)
import Network.HTTP.Types (badRequest400, hContentType, status200)
import Network.Wai
  ( Request(queryString, requestHeaders)
  , Response
  , ResponseReceived
  , lazyRequestBody
  , responseLBS
  )

type instance Input = Request

type instance Output = Response -> IO ResponseReceived

type instance OutputResult = ResponseReceived

instance Interpreter IO where
  onReadImageBS = lazyRequestBody
  onWriteImageBS output bs = output $ responseLBS status200 [] bs
  onGetSize request = do
    let width =
          maybe 0 (read . unpack) (join . lookup "w" . queryString $ request)
        height =
          maybe 0 (read . unpack) (join . lookup "h" . queryString $ request)
        size = max width height
    return $ max width height
  onGetQuality request =
    return $
    maybe (-1) (read . unpack) (join . lookup "q" . queryString $ request)
  onGetImageType request = do
    let maybeContentType = lookup hContentType . requestHeaders $ request
    case maybeContentType of
      Just contentType ->
        case contentType of
          "image/jpeg" -> return Jpeg
          "image/png" -> return Png
          _ -> return Unsupported
      Nothing -> return Unsupported
  onReportError output message =
    output $
    responseLBS badRequest400 [("Content-Type", "text/plain")] (pack message)
