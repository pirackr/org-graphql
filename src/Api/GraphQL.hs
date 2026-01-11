{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Api.GraphQL (execute) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Data.Morpheus.Server (App, deriveApp, runApp)
import Data.Morpheus.Server.Types (GQLType, RootResolver (..), Undefined, defaultRootResolver)

import qualified OrgBackend

newtype Query m = Query
  { hello :: m Text
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {hello = pure (pack OrgBackend.hello)}
    , mutationResolver = mutationResolver defaultRootResolver
    , subscriptionResolver = subscriptionResolver defaultRootResolver
    }

api :: App () IO
api = deriveApp rootResolver

execute :: ByteString -> IO ByteString
execute = runApp api
