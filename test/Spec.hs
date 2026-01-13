module Main (main) where

import qualified Api.GraphQL as GraphQL
import Control.Exception (bracket_)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.List (isInfixOf)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import qualified Org.ParserSpec
import qualified OrgBackend
import System.Directory (setModificationTime)
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

    it "rejects requests without auth when ORG_BACKEND_TOKEN is set" $ do
      let query = "{\"query\":\"{ hello }\"}"
      bracket_ (setEnv "ORG_BACKEND_TOKEN" "secret") (unsetEnv "ORG_BACKEND_TOKEN") $ do
        result <- GraphQL.execute (pack query)
        unpack result `shouldSatisfy` isInfixOf "ORG_BACKEND: unauthorized"

    it "accepts requests with a valid bearer token" $ do
      let query = "{\"authorization\":\"Bearer secret\",\"query\":\"{ hello }\"}"
      bracket_ (setEnv "ORG_BACKEND_TOKEN" "secret") (unsetEnv "ORG_BACKEND_TOKEN") $ do
        result <- GraphQL.execute (pack query)
        unpack result `shouldBe` "{\"data\":{\"hello\":\"hello\"}}"

    it "returns headline properties as json" $ do
      let query =
            "{\"query\":\"{ parseOrg(text: \\\"* Hello\\\\n:PROPERTIES:\\\\n:ID: 123\\\\n:END:\\\\n\\\") { headlines { propertiesJson } } }\"}"
      result <- GraphQL.execute (pack query)
      unpack result
        `shouldBe` "{\"data\":{\"parseOrg\":{\"headlines\":[{\"propertiesJson\":\"{\\\"ID\\\":\\\"123\\\"}\"}]}}}"

    it "returns empty properties as json object" $ do
      let query =
            "{\"query\":\"{ orgFile(path: \\\"emptyprops.org\\\") { headlines { propertiesJson } } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result
          `shouldBe` "{\"data\":{\"orgFile\":{\"headlines\":[{\"propertiesJson\":\"{}\"}]}}}"

    it "reads org files from ORG_BACKEND_ORG_DIR" $ do
      let query =
            "{\"query\":\"{ orgFile(path: \\\"sample.org\\\") { headlines { propertiesJson } } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result
          `shouldBe` "{\"data\":{\"orgFile\":{\"headlines\":[{\"propertiesJson\":\"{\\\"ID\\\":\\\"123\\\"}\"}]}}}"

    it "returns GraphQL errors for missing files" $ do
      let query = "{\"query\":\"{ orgFile(path: \\\"missing.org\\\") { headlines { title } } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result `shouldSatisfy` isInfixOf "ORG_BACKEND: file not found: missing.org"

    it "lists org files under ORG_BACKEND_ORG_DIR" $ do
      let query = "{\"query\":\"{ orgFiles { total items } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result
          `shouldBe` "{\"data\":{\"orgFiles\":{\"total\":5,\"items\":[\"emptyprops.org\",\"invalid.org\",\"sample.org\",\"subdir/child.org\",\"tagged.org\"]}}}"

    it "skips hidden org files" $ do
      let query = "{\"query\":\"{ orgFiles { total items } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result `shouldSatisfy` (not . isInfixOf ".hidden.org")

    it "filters org files by prefix" $ do
      let query = "{\"query\":\"{ orgFiles(prefix: \\\"subdir/\\\") { total items } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result
          `shouldBe` "{\"data\":{\"orgFiles\":{\"total\":1,\"items\":[\"subdir/child.org\"]}}}"

    it "treats prefix without trailing slash as a directory prefix" $ do
      let query = "{\"query\":\"{ orgFiles(prefix: \\\"subdir\\\") { total items } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result
          `shouldBe` "{\"data\":{\"orgFiles\":{\"total\":1,\"items\":[\"subdir/child.org\"]}}}"

    it "filters org files by tags" $ do
      let query = "{\"query\":\"{ orgFiles(prefix: \\\"tagged.org\\\", filterTags: [\\\"work\\\"]) { total items } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result
          `shouldBe` "{\"data\":{\"orgFiles\":{\"total\":1,\"items\":[\"tagged.org\"]}}}"

    it "filters org files by todo" $ do
      let query = "{\"query\":\"{ orgFiles(prefix: \\\"tagged.org\\\", filterTodo: \\\"TODO\\\") { total items } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result
          `shouldBe` "{\"data\":{\"orgFiles\":{\"total\":1,\"items\":[\"tagged.org\"]}}}"

    it "skips invalid files when filtering by tags" $ do
      let query = "{\"query\":\"{ orgFiles(filterTags: [\\\"work\\\"]) { total items } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result `shouldSatisfy` isInfixOf "\"total\":1"

    it "lists org files non-recursively" $ do
      let query = "{\"query\":\"{ orgFiles(recursive: false) { total items } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result
          `shouldBe` "{\"data\":{\"orgFiles\":{\"total\":4,\"items\":[\"emptyprops.org\",\"invalid.org\",\"sample.org\",\"tagged.org\"]}}}"

    it "includes hidden org files when requested" $ do
      let query = "{\"query\":\"{ orgFiles(recursive: false, includeHidden: true) { total items } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result
          `shouldBe` "{\"data\":{\"orgFiles\":{\"total\":5,\"items\":[\".hidden.org\",\"emptyprops.org\",\"invalid.org\",\"sample.org\",\"tagged.org\"]}}}"

    it "paginates org files with offset and limit" $ do
      let query = "{\"query\":\"{ orgFiles(offset: 1, limit: 1) { total items } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result
          `shouldBe` "{\"data\":{\"orgFiles\":{\"total\":5,\"items\":[\"invalid.org\"]}}}"

    it "sorts org files by mtime" $ do
      let query = "{\"query\":\"{ orgFiles(sort: MTIME) { items } }\"}"
          oldest = UTCTime (fromGregorian 2023 12 31) (secondsToDiffTime 0)
          older = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)
          middle = UTCTime (fromGregorian 2024 1 2) (secondsToDiffTime 0)
          newer = UTCTime (fromGregorian 2024 1 3) (secondsToDiffTime 0)
          newest = UTCTime (fromGregorian 2024 1 4) (secondsToDiffTime 0)
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        setModificationTime "test/fixtures/emptyprops.org" oldest
        setModificationTime "test/fixtures/invalid.org" older
        setModificationTime "test/fixtures/sample.org" newer
        setModificationTime "test/fixtures/tagged.org" middle
        setModificationTime "test/fixtures/subdir/child.org" newest
        result <- GraphQL.execute (pack query)
        unpack result
          `shouldBe` "{\"data\":{\"orgFiles\":{\"items\":[\"subdir/child.org\",\"sample.org\",\"tagged.org\",\"invalid.org\",\"emptyprops.org\"]}}}"

    it "sorts org files by name descending" $ do
      let query = "{\"query\":\"{ orgFiles(sort: NAME, sortDirection: DESC) { items } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result
          `shouldBe` "{\"data\":{\"orgFiles\":{\"items\":[\"tagged.org\",\"subdir/child.org\",\"sample.org\",\"invalid.org\",\"emptyprops.org\"]}}}"

    it "returns GraphQL errors for invalid orgFiles prefix" $ do
      let query = "{\"query\":\"{ orgFiles(prefix: \\\"../\\\") { items } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result `shouldSatisfy` isInfixOf "ORG_BACKEND: invalid prefix:"

    it "returns GraphQL errors for parse failures" $ do
      let query = "{\"query\":\"{ orgFile(path: \\\"invalid.org\\\") { headlines { title } } }\"}"
      bracket_ (setEnv "ORG_BACKEND_ORG_DIR" "test/fixtures") (unsetEnv "ORG_BACKEND_ORG_DIR") $ do
        result <- GraphQL.execute (pack query)
        unpack result `shouldSatisfy` isInfixOf "ORG_BACKEND: parse error:"
