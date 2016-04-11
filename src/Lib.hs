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

type API = "login" :> ReqBody '[JSON] User :> Post '[JSON] User

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
    h <- R.connect "172.17.0.2" 28015 Nothing
    users <- RNC.run h $ RNC.table "users" RNC.# RNC.getAll "email" [RNC.str email] :: IO (RNC.Cursor User)
    c <- RNC.collect users
    return $ head c

postLogin user = do
    liftIO $ getUser (email user)

server :: Server API
server = postLogin
