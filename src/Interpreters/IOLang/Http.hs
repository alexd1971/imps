{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Interpreters.IOLang.Http
  (
  ) where

import Control.Monad (join, when)
import Control.Monad.Writer (MonadWriter(tell), Writer, execWriter)
import DSL.IOLang
  ( ImageType(..)
  , Input
  , InputData(..)
  , Interpreter(..)
  , Output
  , OutputResult
  )
import DSL.ImpLang (Height, Quality(..), ResizeRule(..), Width)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy as BL (ByteString, fromStrict, null)
import Data.Maybe (fromJust)
import Data.Text (Text, toLower)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types (badRequest400, hContentType, status200)
import Network.Wai
  ( Request(queryString, requestHeaders)
  , Response
  , ResponseReceived
  , lazyRequestBody
  , responseLBS
  )
import Text.Read (readMaybe)

type instance Input = Request

type instance Output = Response -> IO ResponseReceived

type instance OutputResult = ResponseReceived

instance Interpreter IO where
  onReadInput request = do
    let width = getWidth request
        height = getHeight request
        quality' = getQuality request
        resizeRule = getResizeRule request
        imageType = getImageType request
    imageBS <- lazyRequestBody request
    let errors =
          execWriter $ do
            checkSize width height
            checkQuality quality'
            checkResizeRule resizeRule
            checkImageType imageType
            checkImageBS imageBS
    if errors == mempty
      then do
        let quality = fromJust quality'
            inputData = InputData {..}
        return (Right inputData)
      else return (Left errors)
  onWriteImageBS output bs = output $ responseLBS status200 [] bs
  onReportError output message =
    output $
    responseLBS
      badRequest400
      [("Content-Type", "text/plain")]
      (fromStrict . encodeUtf8 $ message)

getWidth :: Request -> Maybe Width
getWidth request =
  (join . lookup "w" . queryString $ request) >>= (readMaybe . unpack)

getHeight :: Request -> Maybe Height
getHeight request =
  (join . lookup "h" . queryString $ request) >>= (readMaybe . unpack)

getQuality :: Request -> Maybe Quality
getQuality request = do
  let param = lookup "q" . queryString $ request
  case param of
    Nothing -> return Default
    Just mVal ->
      case mVal of
        Just bs -> do
          val <- readMaybe . unpack $ bs
          return $ Quality val
        Nothing -> return Default

getResizeRule :: Request -> Maybe ResizeRule
getResizeRule request = do
  let param = lookup "r" . queryString $ request
  case param of
    Nothing -> return Contain
    Just mVal ->
      case mVal of
        Just bs -> do
          let val = decodeUtf8 bs
          case toLower val of
            "cover" -> return Cover
            "contain" -> return Contain
            _ -> Nothing
        Nothing -> return Contain

getImageType :: Request -> Maybe ImageType
getImageType request = do
  let maybeContentType = lookup hContentType . requestHeaders $ request
  case maybeContentType of
    Just contentType ->
      case contentType of
        "image/jpeg" -> return Jpeg
        "image/png" -> return Png
        _ -> Nothing
    Nothing -> Nothing

checkSize :: Maybe Width -> Maybe Height -> Writer Text ()
checkSize Nothing Nothing =
  tell "Incorrect size parameters. You have to set at least one dimention.\n"
checkSize _ _ = return ()

checkQuality :: Maybe Quality -> Writer Text ()
checkQuality quality = do
  case quality of
    Just (Quality q) -> when (q < 0 || q > 100) $ tell message
    Just Default -> return ()
    Nothing -> tell message
  where
    message =
      "Incorrect quality. Quality must be an integer between 0 and 100.\n"

checkResizeRule :: Maybe ResizeRule -> Writer Text ()
checkResizeRule rule = do
  case rule of
    Just Contain -> return ()
    Just Cover -> return ()
    Nothing ->
      tell
        "Unsupported resize rule. Possible values: contain, cover. Default: contain.\n"

checkImageType :: Maybe ImageType -> Writer Text ()
checkImageType t = do
  case t of
    Just Jpeg -> return ()
    Just Png -> return ()
    Nothing ->
      tell
        "Image type not set or unsupported. You have to set Content-Type header to supported image type: image/jpeg or image/png.\n"

checkImageBS :: ByteString -> Writer Text ()
checkImageBS bs
  | BL.null bs = tell "No image.\n"
  | otherwise = return ()
