module Main (main) where

import Data.ByteString.Lazy.Char8 (pack, unpack)
import Test.Hspec

import qualified Api.GraphQL as GraphQL
import qualified OrgBackend

main :: IO ()
main = hspec $ do
  describe "OrgBackend.hello" $ do
    it "returns a greeting" $ do
      OrgBackend.hello `shouldBe` "hello"

  describe "GraphQL" $ do
    it "returns hello field" $ do
      result <- GraphQL.execute (pack "{\"query\":\"{ hello }\"}")
      unpack result `shouldBe` "{\"data\":{\"hello\":\"hello\"}}"
