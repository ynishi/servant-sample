module Usecase
  ( add
  , Usecase.all
  , Ds(..)
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

newtype Ds = DsTodo
  { dsTitle :: String
  }

add :: (Store a) => a -> Req -> IO Res
add st (Req s) = do
  store st ds
  return . ResOne . ResTodo . title $ todo
  where
    todo = Todo 0 s
    ds = DsTodo $ title todo

all :: (Store a) => a -> Req -> IO Res
all st ReqAll = do
  dsTodos <- fetchAll st
  return $ Res $ map (ResTodo . dsTitle) dsTodos

class Store a where
  store :: a -> Ds -> IO ()
  store _ _ = return ()
  fetchAll :: a -> IO [Ds]
  fetchAll _ = return []

-- DefaultStore is convinient to start
instance Store DefaultStore where
  store (DefaultStore db) ds = do
    liftIO . atomically . modifyTVar db $ (\ts -> ds : ts)
    print $ "writeTVar: " ++ dsTitle ds
    return ()
  fetchAll (DefaultStore db) = liftIO $ readTVarIO db

newtype DefaultStore =
  DefaultStore (TVar [Ds])
