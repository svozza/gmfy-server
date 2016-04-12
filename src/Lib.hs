{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Maybe
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either
import qualified Database.RethinkDB as R
import qualified Database.RethinkDB.NoClash as RNC
import Servant
import Prelude hiding (id)
import qualified Data.Aeson.Parser

type API = "login" :> ReqBody '[JSON] User :> Post '[] ()

data User = User
    {
        id :: Maybe String,
        email :: String
    } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User
instance RNC.ToDatum User
instance RNC.FromDatum User

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

-- getUser :: String -> IO Maybe User
getUser email = do
    h <- R.connect "172.17.0.1" 28015 Nothing
    users <- RNC.run h $ RNC.table "users" RNC.# RNC.getAll "email" [RNC.str email] :: IO (RNC.Cursor User)
    res <- RNC.collect users
    case res of
        []      ->  return Nothing
        (x:_)   ->  return (Just x)


postLogin user = do
    m <- liftIO $ getUser (email user)
    case m of
          Nothing -> left err403
          Just x  -> right ()

server :: Server API
server = postLogin
