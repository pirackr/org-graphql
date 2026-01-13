module Main (main) where

import qualified Api.GraphQL as GraphQL
import Control.Exception (bracket_)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.List (isInfixOf)
import qualified Org.ParserSpec
import qualified OrgBackend
import System.Environment (setEnv, unsetEnv)
import Test.Hspec

main :: IO ()
main = hspec $ do
  Org.ParserSpec.spec
  describe "OrgBackend.hello" $ do
    it "returns a greeting" $ do
      OrgBackend.hello `shouldBe` "hello"

  describe "GraphQL" $ do
    it "returns hello field" $ do
      result <- GraphQL.execute (pack "{\"query\":\"{ hello }\"}")
      unpack result `shouldBe` "{\"data\":{\"hello\":\"hello\"}}"
