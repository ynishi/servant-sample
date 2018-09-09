{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
  ( startApp
  , app
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text                as Text
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Usecase                  as UC

$(deriveJSON defaultOptions ''ResTodo)

$(deriveJSON defaultOptions ''Res)

$(deriveJSON defaultOptions ''Req)

instance FromHttpApiData Req where
  parseUrlPiece = Right . Req . Text.unpack

type API
   = Get '[ PlainText] String
   :<|> "todos" :> "all" :> Get '[ JSON] Res
   :<|> "todos" :> ReqBody '[ JSON] Req :> Post '[ JSON] Res

startApp :: IO ()
startApp = do
  tVar <- atomically $ newTVar []
  let db = DefaultStore tVar
  run 8080 $ app db

app :: (Store a) => a -> Application
app db = serve api (server db)

api :: Proxy API
api = Proxy

server :: (Store a) => a -> Server API
server db = hello
  :<|> todoAll
  :<|> todoPost
  where
    hello = return "Hello"
    todoAll = liftIO $ UC.all db ReqAll
    todoPost req = liftIO $ UC.add db req
