{-# LANGUAGE OverloadedStrings #-}

module Org.Parser (parseOrgText) where

import Data.Attoparsec.Text
  ( Parser
  , char
  , many1
  , parseOnly
  , skipSpace
  , string
  , takeText
  , takeTill
  )
import Data.Char (isAlphaNum, isDigit, isLetter, isUpper, toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Org.Types (OrgFile (..), OrgHeadline (..))

data HeadlineBuilder = HeadlineBuilder
  { hbId :: Text
  , hbLevel :: Int
  , hbTitle :: Text
  , hbTodo :: Maybe Text
  , hbTags :: [Text]
  , hbScheduled :: Maybe Text
  , hbProperties :: Map Text Text
  , hbBodyRev :: [Text]
  , hbChildrenRev :: [OrgHeadline]
  }

parseOrgText :: Text -> Either Text OrgFile
parseOrgText input = do
  (rootsRev, stack, inProps) <- foldl parseLine (Right ([], [], False)) (Text.lines input)
  let (finalRootsRev, finalStack) = closeUntil 0 (rootsRev, stack)
      roots = reverse finalRootsRev
  if inProps
    then Left "Unclosed property drawer"
    else
      if null finalStack
        then pure (OrgFile roots)
        else Left "Unclosed headlines"
  where
    parseLine acc line = do
      (rootsRev, stack, inProps) <- acc
      let lineStart = Text.stripStart line
          lineTrimmed = Text.strip line
      if inProps
        then
          if lineTrimmed == ":END:"
            then Right (rootsRev, stack, False)
            else case parseOnly propertyParser lineTrimmed of
              Left err -> Left (Text.pack err)
              Right prop ->
                Right
                  ( rootsRev
                  , updateTop stack (\b -> b {hbProperties = insertProperty prop (hbProperties b)})
                  , True
                  )
        else
          if lineTrimmed == ":PROPERTIES:"
            then
              if null stack
                then Right (rootsRev, stack, False)
                else Right (rootsRev, stack, True)
            else
              if Text.isPrefixOf "SCHEDULED:" lineTrimmed
                then case parseOnly scheduledParser lineTrimmed of
                  Left err -> Left (Text.pack err)
                  Right scheduled ->
                    Right
                      ( rootsRev
                      , updateTop stack (\b -> b {hbScheduled = Just scheduled})
                      , False
                      )
                else case parseOnly headlineParser lineStart of
                  Right headline ->
                    let level = headlineLevel headline
                        builder = toBuilder headline
                        (rootsRev', stack') = closeUntil level (rootsRev, stack)
                     in Right (rootsRev', builder : stack', False)
                  Left err ->
                    if Text.isPrefixOf "*" lineStart
                      then Left (Text.pack err)
                      else
                        Right
                          ( rootsRev
                          , updateTop stack (\b -> b {hbBodyRev = line : hbBodyRev b})
                          , False
                          )

headlineParser :: Parser OrgHeadline
headlineParser = do
  stars <- many1 (char '*')
  char ' '
  raw <- takeText
  let rawTitle = Text.strip raw
      (titleWithoutTags, tags) = splitTags rawTitle
      (todo, title) = splitTodo titleWithoutTags
  pure
    OrgHeadline
      { headlineId = slugify title
      , headlineLevel = length stars
      , headlineTitle = title
      , headlineTodo = todo
      , headlineTags = tags
      , headlineScheduled = Nothing
      , headlineProperties = Map.empty
      , headlineBody = []
      , headlineChildren = []
      }

scheduledParser :: Parser Text
scheduledParser = do
  _ <- string "SCHEDULED:"
  skipSpace
  _ <- char '<'
  timestamp <- takeTill (== '>')
  _ <- char '>'
  pure timestamp

propertyParser :: Parser (Text, Text)
propertyParser = do
  _ <- char ':'
  key <- takeTill (== ':')
  _ <- char ':'
  value <- takeText
  if Text.null key
    then fail "Property key is empty"
    else pure (key, Text.strip value)

toBuilder :: OrgHeadline -> HeadlineBuilder
toBuilder headline =
  HeadlineBuilder
    { hbId = headlineId headline
    , hbLevel = headlineLevel headline
    , hbTitle = headlineTitle headline
    , hbTodo = headlineTodo headline
    , hbTags = headlineTags headline
    , hbScheduled = headlineScheduled headline
    , hbProperties = Map.empty
    , hbBodyRev = []
    , hbChildrenRev = []
    }

updateTop :: [HeadlineBuilder] -> (HeadlineBuilder -> HeadlineBuilder) -> [HeadlineBuilder]
updateTop stack f =
  case stack of
    [] -> []
    top : rest -> f top : rest

finalize :: HeadlineBuilder -> OrgHeadline
finalize builder =
  OrgHeadline
    { headlineId = hbId builder
    , headlineLevel = hbLevel builder
    , headlineTitle = hbTitle builder
    , headlineTodo = hbTodo builder
    , headlineTags = hbTags builder
    , headlineScheduled = hbScheduled builder
    , headlineProperties = hbProperties builder
    , headlineBody = reverse (hbBodyRev builder)
    , headlineChildren = reverse (hbChildrenRev builder)
    }

insertProperty :: (Text, Text) -> Map Text Text -> Map Text Text
insertProperty (key, value) = Map.insert key value

closeUntil :: Int -> ([OrgHeadline], [HeadlineBuilder]) -> ([OrgHeadline], [HeadlineBuilder])
closeUntil level (rootsRev, stack) =
  case stack of
    [] -> (rootsRev, [])
    top : rest ->
      if hbLevel top >= level
        then
          let node = finalize top
           in case rest of
                parent : remaining ->
                  let parent' = parent {hbChildrenRev = node : hbChildrenRev parent}
                   in closeUntil level (rootsRev, parent' : remaining)
                [] -> closeUntil level (node : rootsRev, [])
        else (rootsRev, stack)

slugify :: Text -> Text
slugify = Text.dropAround (== '-') . Text.concatMap toSlugChar . Text.toLower
  where
    toSlugChar c
      | isAlphaNum c = Text.singleton c
      | c == ' ' = "-"
      | otherwise = ""

splitTags :: Text -> (Text, [Text])
splitTags input =
  case Text.words input of
    [] -> (input, [])
    wordsList ->
      let lastWord = last wordsList
       in case parseTagsWord lastWord of
            Nothing -> (input, [])
            Just tags ->
              (Text.unwords (init wordsList), tags)

parseTagsWord :: Text -> Maybe [Text]
parseTagsWord word =
  if Text.length word >= 2 && Text.head word == ':' && Text.last word == ':'
    then
      let stripped = Text.dropAround (== ':') word
          tags = filter (not . Text.null) (Text.splitOn ":" stripped)
       in if null tags then Nothing else Just tags
    else Nothing

splitTodo :: Text -> (Maybe Text, Text)
splitTodo input =
  case Text.words input of
    [] -> (Nothing, input)
    firstWord : rest ->
      if isTodoWord firstWord
        then (Just firstWord, Text.unwords rest)
        else (Nothing, input)

isTodoWord :: Text -> Bool
isTodoWord word =
  let normalized = Text.toUpper word
   in normalized `elem` todoKeywords || isAllCapsWord word

todoKeywords :: [Text]
todoKeywords =
  [ "TODO"
  , "NEXT"
  , "WAIT"
  , "DONE"
  , "CANCELLED"
  , "IN-PROGRESS"
  ]

isAllCapsWord :: Text -> Bool
isAllCapsWord word =
  let isAllowed c = isUpper c || isDigit c || c == '-' || c == '_'
   in not (Text.null word) && Text.any isLetter word && Text.all isAllowed word
