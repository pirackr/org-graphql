{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Api.GraphQL (execute) where

import Prelude hiding (id)

import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import Data.Morpheus.Server (App, deriveApp, runApp)
import Data.Morpheus.Server.Types
  ( Arg (..)
  , GQLType
  , RootResolver (..)
  , Undefined
  , defaultRootResolver
  )
import Data.Morpheus.Server.Resolvers (ResolverQ)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath
  ( isRelative
  , makeRelative
  , splitDirectories
  , takeExtension
  , takeFileName
  , (</>)
  )

import qualified Org.Parser as OrgParser
import qualified Org.Types as OrgTypes
import qualified OrgBackend

data Query m = Query
  { hello :: m Text
  , parseOrg :: Arg "text" Text -> m OrgFileGQL
  , orgFile :: Arg "path" Text -> m OrgFileGQL
  , orgFiles :: m [Text]
  }
  deriving (Generic, GQLType)

newtype OrgFileGQL = OrgFileGQL
  { headlines :: [HeadlineGQL]
  }
  deriving (Generic, GQLType)

data HeadlineGQL = HeadlineGQL
  { id :: Text
  , level :: Int
  , title :: Text
  , todo :: Maybe Text
  , tags :: [Text]
  , scheduled :: Maybe Text
  , properties :: [PropertyGQL]
  , body :: [Text]
  , children :: [HeadlineGQL]
  }
  deriving (Generic, GQLType)

data PropertyGQL = PropertyGQL
  { key :: Text
  , value :: Text
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver =
        Query
          { hello = pure (pack OrgBackend.hello)
          , parseOrg = parseOrgResolver
          , orgFile = orgFileResolver
          , orgFiles = orgFilesResolver
          }
    , mutationResolver = mutationResolver defaultRootResolver
    , subscriptionResolver = subscriptionResolver defaultRootResolver
    }

api :: App () IO
api = deriveApp rootResolver

execute :: ByteString -> IO ByteString
execute = runApp api

parseOrgResolver :: Arg "text" Text -> ResolverQ () IO OrgFileGQL
parseOrgResolver (Arg input) =
  case OrgParser.parseOrgText input of
    Left err -> fail (withPrefix ("parse error: " <> err))
    Right orgFile -> pure (toOrgFileGQL orgFile)

orgFileResolver :: Arg "path" Text -> ResolverQ () IO OrgFileGQL
orgFileResolver (Arg pathText) = do
  root <- liftIO getOrgRoot
  path <-
    case validatePath pathText of
      Left err -> fail (withPrefix ("invalid path: " <> err))
      Right ok -> pure ok
  let fullPath = root </> path
  exists <- liftIO (doesFileExist fullPath)
  if not exists
    then fail (withPrefix ("file not found: " <> Text.pack path))
    else do
      content <- liftIO (TextIO.readFile fullPath)
      case OrgParser.parseOrgText content of
        Left err -> fail (withPrefix ("parse error: " <> err))
        Right orgFile -> pure (toOrgFileGQL orgFile)

orgFilesResolver :: ResolverQ () IO [Text]
orgFilesResolver = do
  root <- liftIO getOrgRoot
  exists <- liftIO (doesDirectoryExist root)
  if not exists
    then fail (withPrefix ("org dir not found: " <> Text.pack root))
    else do
      paths <- liftIO (listOrgFiles root)
      pure (map Text.pack (sort paths))

toOrgFileGQL :: OrgTypes.OrgFile -> OrgFileGQL
toOrgFileGQL orgFile =
  OrgFileGQL
    { headlines = map toHeadlineGQL (OrgTypes.orgFileHeadlines orgFile)
    }

toHeadlineGQL :: OrgTypes.OrgHeadline -> HeadlineGQL
toHeadlineGQL headline =
  HeadlineGQL
    { id = OrgTypes.headlineId headline
    , level = OrgTypes.headlineLevel headline
    , title = OrgTypes.headlineTitle headline
    , todo = OrgTypes.headlineTodo headline
    , tags = OrgTypes.headlineTags headline
    , scheduled = OrgTypes.headlineScheduled headline
    , properties = map toPropertyGQL (Map.toList (OrgTypes.headlineProperties headline))
    , body = OrgTypes.headlineBody headline
    , children = map toHeadlineGQL (OrgTypes.headlineChildren headline)
    }

toPropertyGQL :: (Text, Text) -> PropertyGQL
toPropertyGQL (propKey, propValue) =
  PropertyGQL
    { key = propKey
    , value = propValue
    }

withPrefix :: Text -> String
withPrefix message = Text.unpack ("ORG_BACKEND: " <> message)

getOrgRoot :: IO FilePath
getOrgRoot = do
  env <- lookupEnv "ORG_BACKEND_ORG_DIR"
  pure (fromMaybe "org" env)

validatePath :: Text -> Either Text FilePath
validatePath rawPath =
  let path = Text.unpack (Text.strip rawPath)
   in if null path
        then Left "empty path"
        else
          if isRelative path && not (".." `elem` splitDirectories path)
            then Right path
            else Left "path must be relative and not contain .."

listOrgFiles :: FilePath -> IO [FilePath]
listOrgFiles root = go root
  where
    go dir = do
      entries <- listDirectory dir
      paths <- traverse (resolveEntry dir) (filter (not . isHidden) entries)
      pure (concat paths)
    resolveEntry dir entry = do
      let fullPath = dir </> entry
      isDir <- doesDirectoryExist fullPath
      if isDir
        then go fullPath
        else
          if takeExtension entry == ".org"
            then pure [makeRelative root fullPath]
            else pure []
    isHidden entry =
      case takeFileName entry of
        '.' : _ -> True
        _ -> False
