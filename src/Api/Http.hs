{-# LANGUAGE OverloadedStrings #-}

module Api.Http (injectAuthorizationHeader) where

import Data.Aeson (Value (..), decode, encode)
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (insert)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

injectAuthorizationHeader :: Maybe Text -> ByteString -> ByteString
injectAuthorizationHeader maybeAuth body =
  case maybeAuth of
    Nothing -> body
    Just auth ->
      case decode body of
        Just (Object obj) ->
          encode (Object (insert (fromText "authorization") (String auth) obj))
        _ -> body
