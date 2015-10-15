{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import qualified Data.Text as T
import           Data.Aeson
import           Snap.Snaplet.PostgresqlSimple

data Todo = Todo
  { todoId   :: Int
  , todoText :: T.Text
  }

instance FromRow Todo where
  fromRow = Todo <$> field
                 <*> field

instance ToJSON Todo where
  toJSON (Todo tId tText) = object [ "id" .= tId, "text" .= tText ]
