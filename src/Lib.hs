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
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Database.RethinkDB as R
import qualified Database.RethinkDB.NoClash as RNC
import Servant
import Prelude hiding (id)
import qualified Data.Aeson.Parser

data Login = Login
  { emailAddr        :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Login
instance FromJSON Login

type API = "login" :> ReqBody '[JSON] Login :> Post '[JSON] User

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

getUser email = do
    h <- R.connect "172.17.0.1" 28015 Nothing
    users <- RNC.run h $ RNC.table "users" RNC.# RNC.getAll "email" [RNC.str email] :: IO (RNC.Cursor User)
    c <- RNC.collect users
    return $ head c

postLogin _ = do
    liftIO $ getUser "svozza@fake.com"

server :: Server API
server = postLogin
