{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Inputs (PropertyInput (..)) where

import Data.Text (Text)
import Data.Morpheus.Server.Types (GQLType)
import GHC.Generics (Generic)

data PropertyInput = PropertyInput
  { name :: Text
  , value :: Text
  }
  deriving (Generic, GQLType)
