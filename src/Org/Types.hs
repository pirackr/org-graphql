module Org.Types
  ( OrgFile (..)
  , OrgHeadline (..)
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)

data OrgFile = OrgFile
  { orgFilePreamble :: [Text]
  , orgFileHeadlines :: [OrgHeadline]
  }
  deriving (Eq, Show)

data OrgHeadline = OrgHeadline
  { headlineId :: Text
  , headlineLevel :: Int
  , headlineTitle :: Text
  , headlineTodo :: Maybe Text
  , headlineTags :: [Text]
  , headlineScheduled :: Maybe Text
  , headlineProperties :: Map Text Text
  , headlineBody :: [Text]
  , headlineChildren :: [OrgHeadline]
  }
  deriving (Eq, Show)
