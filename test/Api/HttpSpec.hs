{-# LANGUAGE OverloadedStrings #-}

module Api.HttpSpec (spec) where

import Data.Aeson (Value (..), decode)
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (lookup)
import Data.ByteString.Lazy.Char8 (pack)
import Test.Hspec

import qualified Api.Http as Http

spec :: Spec
spec = do
  describe "Api.Http.injectAuthorizationHeader" $ do
    it "injects authorization into request JSON" $ do
      let body = pack "{\"query\":\"{ hello }\"}"
          updated = Http.injectAuthorizationHeader (Just "Bearer token") body
      case decode updated of
        Nothing -> expectationFailure "expected valid JSON"
        Just (Object obj) ->
          lookup (fromText "authorization") obj `shouldBe` Just (String "Bearer token")
        _ -> expectationFailure "expected JSON object"
