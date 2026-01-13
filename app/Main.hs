{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (status200)
import Network.Wai (Application, Request (..), Response, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

import qualified Api.GraphQL as GraphQL
import qualified Api.Http as Http

main :: IO ()
main = do
  port <- getPort
  run port app

app :: Application
app req respond = do
  body <- strictRequestBody req
  let authHeader = lookup "authorization" (requestHeaders req)
      authText = fmap decodeUtf8 authHeader
      requestBody = Http.injectAuthorizationHeader authText body
  result <- GraphQL.execute requestBody
  respond (jsonResponse result)

jsonResponse :: ByteString -> Response
jsonResponse body =
  responseLBS status200 [("Content-Type", "application/json")] body

getPort :: IO Int
getPort = do
  env <- lookupEnv "PORT"
  pure (maybe 8080 read env)
