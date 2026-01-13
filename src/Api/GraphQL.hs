{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Api.GraphQL (execute) where

import Prelude hiding (id)

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), encode, eitherDecode, object, withObject, (.:?), (.=))
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf, isSuffixOf, sort, sortOn)
import Data.Ord (Down (..))
import Data.Morpheus.Server (App, deriveApp, runApp)
import Data.Morpheus.Server.Types
  ( Arg (..)
  , GQLType
  , RootResolver (..)
  , Undefined
  , defaultRootResolver
  )
import Data.Morpheus.Server.Resolvers (ResolverM, ResolverQ)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazyEncoding
import GHC.Generics (Generic)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getModificationTime
  , listDirectory
  , removeFile
  )
import System.Environment (lookupEnv)
import System.FilePath
  ( isRelative
  , makeRelative
  , splitDirectories
  , takeDirectory
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
  , orgFiles ::
      Arg "recursive" (Maybe Bool)
      -> Arg "includeHidden" (Maybe Bool)
      -> Arg "prefix" (Maybe Text)
      -> Arg "offset" (Maybe Int)
      -> Arg "limit" (Maybe Int)
      -> Arg "sort" (Maybe OrgFilesSort)
      -> Arg "sortDirection" (Maybe SortDirection)
      -> Arg "filterTags" (Maybe [Text])
      -> Arg "filterTodo" (Maybe Text)
      -> m OrgFilesGQL
  }
  deriving (Generic, GQLType)

data Mutation m = Mutation
  { writeOrgFile :: Arg "path" Text -> Arg "content" Text -> m Bool
  , deleteOrgFile :: Arg "path" Text -> m Bool
  , updateHeadlineTitle :: Arg "path" Text -> Arg "id" Text -> Arg "title" Text -> m Bool
  , updateHeadlineTodo :: Arg "path" Text -> Arg "id" Text -> Arg "todo" Text -> m Bool
  }
  deriving (Generic, GQLType)

newtype OrgFileGQL = OrgFileGQL
  { headlines :: [HeadlineGQL]
  }
  deriving (Generic, GQLType)

data OrgFilesGQL = OrgFilesGQL
  { total :: Int
  , items :: [Text]
  }
  deriving (Generic, GQLType)

data OrgFilesSort
  = NAME
  | MTIME
  deriving (Generic, GQLType, Eq, Show)

data SortDirection
  = ASC
  | DESC
  deriving (Generic, GQLType, Eq, Show)

data HeadlineGQL = HeadlineGQL
  { id :: Text
  , level :: Int
  , title :: Text
  , todo :: Maybe Text
  , tags :: [Text]
  , scheduled :: Maybe Text
  , propertiesJson :: Text
  , body :: [Text]
  , children :: [HeadlineGQL]
  }
  deriving (Generic)

instance GQLType HeadlineGQL

data AuthEnvelope = AuthEnvelope
  { authAuthorization :: Maybe Text
  , authExtensions :: Maybe AuthExtensions
  }
  deriving (Generic)

newtype AuthExtensions = AuthExtensions
  { authExtAuthorization :: Maybe Text
  }
  deriving (Generic)

instance FromJSON AuthEnvelope where
  parseJSON = withObject "AuthEnvelope" $ \obj -> do
    authAuthorization <- obj .:? "authorization"
    authExtensions <- obj .:? "extensions"
    pure AuthEnvelope {authAuthorization = authAuthorization, authExtensions = authExtensions}

instance FromJSON AuthExtensions where
  parseJSON = withObject "AuthExtensions" $ \obj -> do
    authExtAuthorization <- obj .:? "authorization"
    pure AuthExtensions {authExtAuthorization = authExtAuthorization}

rootResolver :: RootResolver IO () Query Mutation Undefined
rootResolver =
  RootResolver
    { queryResolver =
        Query
          { hello = pure (pack OrgBackend.hello)
          , parseOrg = parseOrgResolver
          , orgFile = orgFileResolver
          , orgFiles = orgFilesResolver
          }
    , mutationResolver =
        Mutation
          { writeOrgFile = writeOrgFileResolver
          , deleteOrgFile = deleteOrgFileResolver
          , updateHeadlineTitle = updateHeadlineTitleResolver
          , updateHeadlineTodo = updateHeadlineTodoResolver
          }
    , subscriptionResolver = subscriptionResolver defaultRootResolver
    }

api :: App () IO
api = deriveApp rootResolver

execute :: ByteString -> IO ByteString
execute input = do
  requiredToken <- lookupEnv "ORG_BACKEND_TOKEN"
  case requiredToken of
    Nothing -> runApp api input
    Just rawToken ->
      if null rawToken
        then runApp api input
        else
          if isAuthorized (Text.pack rawToken) input
            then runApp api input
            else pure unauthorizedResponse

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

orgFilesResolver ::
  Arg "recursive" (Maybe Bool)
  -> Arg "includeHidden" (Maybe Bool)
  -> Arg "prefix" (Maybe Text)
  -> Arg "offset" (Maybe Int)
  -> Arg "limit" (Maybe Int)
  -> Arg "sort" (Maybe OrgFilesSort)
  -> Arg "sortDirection" (Maybe SortDirection)
  -> Arg "filterTags" (Maybe [Text])
  -> Arg "filterTodo" (Maybe Text)
  -> ResolverQ () IO OrgFilesGQL
orgFilesResolver (Arg recursiveArg) (Arg includeHiddenArg) (Arg prefixArg) (Arg offsetArg) (Arg limitArg) (Arg sortArg) (Arg sortDirectionArg) (Arg filterTagsArg) (Arg filterTodoArg) = do
  let recursive = fromMaybe True recursiveArg
      includeHidden = fromMaybe False includeHiddenArg
      offset = max 0 (fromMaybe 0 offsetArg)
      limit = normalizeLimit limitArg
      sortBy = fromMaybe NAME sortArg
      sortDirection = fromMaybe ASC sortDirectionArg
      filterTags = normalizeTags filterTagsArg
      filterTodo = normalizeTodo filterTodoArg
  prefix <-
    case validatePrefix (normalizePrefix prefixArg) of
      Left err -> fail (withPrefix ("invalid prefix: " <> err))
      Right ok -> pure ok
  root <- liftIO getOrgRoot
  exists <- liftIO (doesDirectoryExist root)
  if not exists
    then fail (withPrefix ("org dir not found: " <> Text.pack root))
    else do
      paths <- liftIO (listOrgFiles recursive includeHidden root)
      let prefixFiltered = filter (matchesPrefix prefix) paths
      contentFiltered <-
        if needsContentFilter filterTags filterTodo
          then do
            result <- liftIO (filterPathsByContent root prefixFiltered filterTags filterTodo)
            case result of
              Left err -> fail (withPrefix err)
              Right ok -> pure ok
          else pure prefixFiltered
      sorted <- liftIO (sortPaths sortBy sortDirection root contentFiltered)
      let paged = applyPagination offset limit sorted
          totalCount = length contentFiltered
      pure (OrgFilesGQL {total = totalCount, items = map Text.pack paged})

writeOrgFileResolver :: Arg "path" Text -> Arg "content" Text -> ResolverM () IO Bool
writeOrgFileResolver (Arg pathText) (Arg content) = do
  root <- liftIO getOrgRoot
  path <-
    case validatePath pathText of
      Left err -> fail (withPrefix ("invalid path: " <> err))
      Right ok -> pure ok
  let fullPath = root </> path
  liftIO (createDirectoryIfMissing True (takeDirectory fullPath))
  liftIO (TextIO.writeFile fullPath content)
  pure True

deleteOrgFileResolver :: Arg "path" Text -> ResolverM () IO Bool
deleteOrgFileResolver (Arg pathText) = do
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
      liftIO (removeFile fullPath)
      pure True

updateHeadlineTitleResolver :: Arg "path" Text -> Arg "id" Text -> Arg "title" Text -> ResolverM () IO Bool
updateHeadlineTitleResolver (Arg pathText) (Arg headlineId) (Arg newTitle) = do
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
        Right orgFile ->
          case updateHeadlineTitleInFile headlineId newTitle orgFile of
            Left err -> fail (withPrefix err)
            Right updated -> do
              liftIO (TextIO.writeFile fullPath (renderOrgFile updated))
              pure True

updateHeadlineTodoResolver :: Arg "path" Text -> Arg "id" Text -> Arg "todo" Text -> ResolverM () IO Bool
updateHeadlineTodoResolver (Arg pathText) (Arg headlineId) (Arg newTodo) = do
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
        Right orgFile ->
          case updateHeadlineTodoInFile headlineId newTodo orgFile of
            Left err -> fail (withPrefix err)
            Right updated -> do
              liftIO (TextIO.writeFile fullPath (renderOrgFile updated))
              pure True

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
    , propertiesJson = propertiesToJson (OrgTypes.headlineProperties headline)
    , body = OrgTypes.headlineBody headline
    , children = map toHeadlineGQL (OrgTypes.headlineChildren headline)
    }

propertiesToJson :: Map.Map Text Text -> Text
propertiesToJson props =
  if Map.null props
    then "{}"
    else TextLazy.toStrict (TextLazyEncoding.decodeUtf8 (encode props))

isAuthorized :: Text -> ByteString -> Bool
isAuthorized requiredToken input =
  case eitherDecode input of
    Left _ -> False
    Right envelope ->
      case extractAuthorization envelope of
        Nothing -> False
        Just provided -> matchesToken requiredToken provided

extractAuthorization :: AuthEnvelope -> Maybe Text
extractAuthorization envelope =
  authAuthorization envelope <|> (authExtensions envelope >>= authExtAuthorization)

matchesToken :: Text -> Text -> Bool
matchesToken requiredToken providedRaw =
  let trimmed = Text.strip providedRaw
   in case Text.stripPrefix "Bearer " trimmed of
        Just bearerToken -> Text.strip bearerToken == requiredToken
        Nothing -> trimmed == requiredToken

unauthorizedResponse :: ByteString
unauthorizedResponse =
  encode $ object ["errors" .= [object ["message" .= ("ORG_BACKEND: unauthorized" :: Text)]]]

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

listOrgFiles :: Bool -> Bool -> FilePath -> IO [FilePath]
listOrgFiles recursive includeHidden root = go root
  where
    go dir = do
      entries <- listDirectory dir
      let visibleEntries =
            if includeHidden
              then entries
              else filter (not . isHidden) entries
      paths <- traverse (resolveEntry dir) visibleEntries
      pure (concat paths)
    resolveEntry dir entry = do
      let fullPath = dir </> entry
      isDir <- doesDirectoryExist fullPath
      if isDir
        then
          if recursive
            then go fullPath
            else pure []
        else
          if takeExtension entry == ".org"
            then pure [makeRelative root fullPath]
            else pure []
    isHidden entry =
      case takeFileName entry of
        '.' : _ -> True
        _ -> False

normalizeTags :: Maybe [Text] -> Maybe [Text]
normalizeTags maybeTags =
  case maybeTags of
    Nothing -> Nothing
    Just tags ->
      let cleaned = filter (not . Text.null) (map Text.strip tags)
       in if null cleaned then Nothing else Just cleaned

normalizeTodo :: Maybe Text -> Maybe Text
normalizeTodo maybeTodo =
  case maybeTodo of
    Nothing -> Nothing
    Just raw ->
      let trimmed = Text.strip raw
       in if Text.null trimmed then Nothing else Just trimmed

needsContentFilter :: Maybe [Text] -> Maybe Text -> Bool
needsContentFilter maybeTags maybeTodo =
  case (maybeTags, maybeTodo) of
    (Nothing, Nothing) -> False
    _ -> True

filterPathsByContent :: FilePath -> [FilePath] -> Maybe [Text] -> Maybe Text -> IO (Either Text [FilePath])
filterPathsByContent root paths maybeTags maybeTodo = do
  result <- foldM step (Right []) paths
  pure (fmap reverse result)
  where
    step acc path =
      case acc of
        Left err -> pure (Left err)
        Right matches -> do
          content <- TextIO.readFile (root </> path)
          case OrgParser.parseOrgText content of
            Left _ -> pure (Right matches)
            Right orgFile ->
              if fileMatchesFilters maybeTags maybeTodo orgFile
                then pure (Right (path : matches))
                else pure (Right matches)

fileMatchesFilters :: Maybe [Text] -> Maybe Text -> OrgTypes.OrgFile -> Bool
fileMatchesFilters maybeTags maybeTodo orgFile =
  let headlines = OrgTypes.orgFileHeadlines orgFile
   in anyHeadline (headlineMatchesFilters maybeTags maybeTodo) headlines

anyHeadline :: (OrgTypes.OrgHeadline -> Bool) -> [OrgTypes.OrgHeadline] -> Bool
anyHeadline predicate headlines =
  any
    ( \headline ->
        predicate headline || anyHeadline predicate (OrgTypes.headlineChildren headline)
    )
    headlines

headlineMatchesFilters :: Maybe [Text] -> Maybe Text -> OrgTypes.OrgHeadline -> Bool
headlineMatchesFilters maybeTags maybeTodo headline =
  tagsMatch maybeTags headline && todoMatch maybeTodo headline
  where
    tagsMatch tagsFilter h =
      case tagsFilter of
        Nothing -> True
        Just requiredTags -> all (`elem` OrgTypes.headlineTags h) requiredTags
    todoMatch todoFilter h =
      case todoFilter of
        Nothing -> True
        Just requiredTodo -> OrgTypes.headlineTodo h == Just requiredTodo

normalizePrefix :: Maybe Text -> Maybe FilePath
normalizePrefix maybePrefix =
  case maybePrefix of
    Nothing -> Nothing
    Just raw ->
      let trimmed = Text.unpack (Text.strip raw)
       in if null trimmed
            then Nothing
            else Just trimmed

validatePrefix :: Maybe FilePath -> Either Text (Maybe FilePath)
validatePrefix maybePrefix =
  case maybePrefix of
    Nothing -> Right Nothing
    Just prefix ->
      if isRelative prefix && not (".." `elem` splitDirectories prefix)
        then Right (Just prefix)
        else Left "prefix must be relative and not contain .."

matchesPrefix :: Maybe FilePath -> FilePath -> Bool
matchesPrefix maybePrefix path =
  case maybePrefix of
    Nothing -> True
    Just prefix ->
      if "/" `isSuffixOf` prefix
        then prefix `isPrefixOf` path
        else path == prefix || (prefix <> "/") `isPrefixOf` path

normalizeLimit :: Maybe Int -> Maybe Int
normalizeLimit maybeLimit =
  case maybeLimit of
    Nothing -> Nothing
    Just n ->
      if n <= 0
        then Nothing
        else Just n

applyPagination :: Int -> Maybe Int -> [a] -> [a]
applyPagination offset maybeLimit =
  let dropped = drop offset
   in case maybeLimit of
        Nothing -> dropped
        Just limit -> take limit . dropped

sortPaths :: OrgFilesSort -> SortDirection -> FilePath -> [FilePath] -> IO [FilePath]
sortPaths sortBy sortDirection root paths = do
  let applyDirection :: [a] -> [a]
      applyDirection =
        case sortDirection of
          ASC -> \xs -> xs
          DESC -> reverse
  case sortBy of
    NAME -> pure (applyDirection (sort paths))
    MTIME -> do
      withTimes <-
        traverse
          ( \path -> do
              time <- getModificationTime (root </> path)
              pure (path, time)
          )
          paths
      let sorted = sortOn (Down . snd) withTimes
          ordered = map fst sorted
      pure (applyDirection ordered)

updateHeadlineTitleInFile :: Text -> Text -> OrgTypes.OrgFile -> Either Text OrgTypes.OrgFile
updateHeadlineTitleInFile targetId newTitle orgFile =
  let (updated, updatedHeadlines) = updateHeadlineTitleInHeadlines targetId newTitle (OrgTypes.orgFileHeadlines orgFile)
   in if updated
        then Right orgFile {OrgTypes.orgFileHeadlines = updatedHeadlines}
        else Left ("headline not found: " <> targetId)

updateHeadlineTitleInHeadlines :: Text -> Text -> [OrgTypes.OrgHeadline] -> (Bool, [OrgTypes.OrgHeadline])
updateHeadlineTitleInHeadlines targetId newTitle =
  foldr
    ( \headline (updated, acc) ->
        let (childUpdated, updatedChildren) =
              updateHeadlineTitleInHeadlines targetId newTitle (OrgTypes.headlineChildren headline)
            isTarget = OrgTypes.headlineId headline == targetId
            updatedHeadline =
              if isTarget
                then
                  headline
                    { OrgTypes.headlineId = slugify newTitle
                    , OrgTypes.headlineTitle = newTitle
                    }
                else headline
            updatedWithChildren = updatedHeadline {OrgTypes.headlineChildren = updatedChildren}
         in (updated || isTarget || childUpdated, updatedWithChildren : acc)
    )
    (False, [])

updateHeadlineTodoInFile :: Text -> Text -> OrgTypes.OrgFile -> Either Text OrgTypes.OrgFile
updateHeadlineTodoInFile targetId newTodo orgFile =
  let todoValue = normalizeTodoValue newTodo
      (updated, updatedHeadlines) = updateHeadlineTodoInHeadlines targetId todoValue (OrgTypes.orgFileHeadlines orgFile)
   in if updated
        then Right orgFile {OrgTypes.orgFileHeadlines = updatedHeadlines}
        else Left ("headline not found: " <> targetId)

updateHeadlineTodoInHeadlines :: Text -> Maybe Text -> [OrgTypes.OrgHeadline] -> (Bool, [OrgTypes.OrgHeadline])
updateHeadlineTodoInHeadlines targetId todoValue =
  foldr
    ( \headline (updated, acc) ->
        let (childUpdated, updatedChildren) =
              updateHeadlineTodoInHeadlines targetId todoValue (OrgTypes.headlineChildren headline)
            isTarget = OrgTypes.headlineId headline == targetId
            updatedHeadline =
              if isTarget
                then headline {OrgTypes.headlineTodo = todoValue}
                else headline
            updatedWithChildren = updatedHeadline {OrgTypes.headlineChildren = updatedChildren}
         in (updated || isTarget || childUpdated, updatedWithChildren : acc)
    )
    (False, [])

normalizeTodoValue :: Text -> Maybe Text
normalizeTodoValue raw =
  let trimmed = Text.strip raw
   in if Text.null trimmed then Nothing else Just trimmed

renderOrgFile :: OrgTypes.OrgFile -> Text
renderOrgFile orgFile =
  let lines = concatMap renderHeadline (OrgTypes.orgFileHeadlines orgFile)
   in Text.unlines lines

renderHeadline :: OrgTypes.OrgHeadline -> [Text]
renderHeadline headline =
  let headlineLine = renderHeadlineLine headline
      scheduledLine =
        case OrgTypes.headlineScheduled headline of
          Nothing -> []
          Just timestamp -> ["SCHEDULED: <" <> timestamp <> ">"]
      propertyLines = renderProperties (OrgTypes.headlineProperties headline)
      bodyLines = OrgTypes.headlineBody headline
      childLines = concatMap renderHeadline (OrgTypes.headlineChildren headline)
   in headlineLine : scheduledLine ++ propertyLines ++ bodyLines ++ childLines

renderHeadlineLine :: OrgTypes.OrgHeadline -> Text
renderHeadlineLine headline =
  let stars = Text.replicate (OrgTypes.headlineLevel headline) "*"
      todoPart =
        case OrgTypes.headlineTodo headline of
          Nothing -> ""
          Just todo -> todo <> " "
      tagsPart =
        case OrgTypes.headlineTags headline of
          [] -> ""
          tags -> " :" <> Text.intercalate ":" tags <> ":"
   in stars <> " " <> todoPart <> OrgTypes.headlineTitle headline <> tagsPart

renderProperties :: Map.Map Text Text -> [Text]
renderProperties props =
  if Map.null props
    then []
    else
      let lines = map (\(key, value) -> ":" <> key <> ": " <> value) (Map.toList props)
       in ":PROPERTIES:" : lines ++ [":END:"]

slugify :: Text -> Text
slugify = Text.dropAround (== '-') . Text.concatMap toSlugChar . Text.toLower
  where
    toSlugChar c
      | isAlphaNum c = Text.singleton c
      | c == ' ' = "-"
      | otherwise = ""
