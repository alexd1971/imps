{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Data.Text.Lazy            (Text)
import           Network.HTTP.Types.Status (badRequest400)
import           Resize                    (imageJpegResize, imagePngResize)
import           Web.Scotty                (ActionM, ScottyM, body, header,
                                            liftAndCatchIO, param, post, raw,
                                            rescue, scotty, status, text)

-- Runs service
run :: IO ()
run = scotty 7777 api

-- Creates service api
api :: ScottyM ()
api =
  post "/resize" handleResize

-- Handles resize request
handleResize :: ActionM ()
handleResize = do
  maybeContentType <- header "Content-Type"
  case maybeContentType of
    Nothing -> badRequest "Unknown image format"
    Just contentType -> do
      origImageBS <- body
      requestedWidth <- param "w" `rescue` \_ -> return 0
      requestedHeight <- param "h" `rescue` \_ -> return 0
      if requestedWidth == 0 && requestedHeight == 0
        then badRequest "No requested image size"
        else
          let requestedSize = (requestedWidth, requestedHeight)
           in case contentType of
                "image/jpeg" -> do
                  newImageBS <- liftAndCatchIO $ imageJpegResize requestedSize origImageBS
                  raw newImageBS
                "image/png" -> do
                  newImageBS <- liftAndCatchIO $ imagePngResize requestedSize origImageBS
                  raw newImageBS
                _ -> badRequest "Unsupported image format"

-- Creates Bad Request server response
badRequest :: Text -> ActionM ()
badRequest msg = do
  status badRequest400
  text $ "Bad Request: " <> msg
