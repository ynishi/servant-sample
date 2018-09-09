module Usecase
  ( add
  , Usecase.all
  , DefaultStore(..)
  , Req(..)
  , Res(..)
  , ResTodo(..)
  , Store(..)
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Todo

data Req
  = Req String
  | ReqAll

data Res
  = Res [ResTodo]
  | ResOne ResTodo

newtype ResTodo = ResTodo
  { resTitle :: String
  }

add :: (Store a) => a -> Req -> IO Res
add st (Req s) = do
  store st todo
  return . ResOne . ResTodo . title $ todo
  where
    todo = Todo 0 s

all :: (Store a) => a -> Req -> IO Res
all st ReqAll = do
  todos <- fetchAll st
  return $ Res $ map (ResTodo . title) todos

class Store a where
  store :: a -> Todo -> IO ()
  store _ _ = return ()
  fetchAll :: a -> IO Todos
  fetchAll _ = return []

instance Store DefaultStore where
  store (DefaultStore db) todo = do
    liftIO . atomically . modifyTVar db $ (\ts -> todo : ts)
    print $ "writeTVar: " ++ title todo
    return ()
  fetchAll (DefaultStore db) = liftIO $ readTVarIO db

newtype DefaultStore =
  DefaultStore (TVar Todos)
