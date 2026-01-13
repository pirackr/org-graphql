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

    it "returns headline properties" $ do
      let query =
            "{\"query\":\"{ parseOrg(text: \\\"* Hello\\\\n:PROPERTIES:\\\\n:ID: 123\\\\n:END:\\\\n\\\") { headlines { properties { key value } } } }\"}"
      result <- GraphQL.execute (pack query)
      unpack result
        `shouldBe` "{\"data\":{\"parseOrg\":{\"headlines\":[{\"properties\":[{\"key\":\"ID\",\"value\":\"123\"}]}]}}}"

    it "reads org files from ORG_BACKEND_ORG_DIR" $ do
      let query =
            "{\"query\":\"{ orgFile(path: \\\"sample.org\\\") { headlines { properties { key value } } } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result
          `shouldBe` "{\"data\":{\"orgFile\":{\"headlines\":[{\"properties\":[{\"key\":\"ID\",\"value\":\"123\"}]}]}}}"

    it "returns GraphQL errors for missing files" $ do
      let query = "{\"query\":\"{ orgFile(path: \\\"missing.org\\\") { headlines { title } } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result `shouldSatisfy` isInfixOf "ORG_BACKEND: file not found: missing.org"

    it "lists org files under ORG_BACKEND_ORG_DIR" $ do
      let query = "{\"query\":\"{ orgFiles }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result
          `shouldBe` "{\"data\":{\"orgFiles\":[\"invalid.org\",\"sample.org\"]}}"

    it "skips hidden org files" $ do
      let query = "{\"query\":\"{ orgFiles }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result `shouldSatisfy` (not . isInfixOf ".hidden.org")

    it "returns GraphQL errors for parse failures" $ do
      let query = "{\"query\":\"{ orgFile(path: \\\"invalid.org\\\") { headlines { title } } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result `shouldSatisfy` isInfixOf "ORG_BACKEND: parse error:"
