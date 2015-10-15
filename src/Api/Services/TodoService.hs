{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Api.Services.TodoService where

import Api.Types
import Control.Lens
import Control.Monad.State.Class
import Data.Aeson
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.SqliteSimple
import qualified Data.ByteString.Char8 as B

data TodoService = TodoService { _db :: Snaplet Sqlite }

makeLenses ''TodoService

todoRoutes :: [(B.ByteString, Handler b TodoService ())]
todoRoutes = [("/", method GET getTodos), ("/", method POST createTodo)]

createTodo :: Handler b TodoService ()
createTodo = do
  todoTextParam <- getPostParam "text"
  execute "INSERT INTO todos (text) VALUES (?)" (Only todoTextParam)
  modifyResponse $ setResponseCode 201

getTodos :: Handler b TodoService ()
getTodos = do
  todos <- query_ "SELECT * FROM todos"
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ (todos :: [Todo])

todoServiceInit :: SnapletInit b TodoService
todoServiceInit = makeSnaplet "todos" "Todo Service" Nothing $ do
  d <- nestSnaplet "db" db sqliteInit
  addRoutes todoRoutes
  return $ TodoService d

instance HasSqlite (Handler b TodoService) where
  getSqliteState = with db get
