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
import qualified Database.RethinkDB as R
import qualified Database.RethinkDB.NoClash as RNC
import Servant
import Prelude hiding (id)
import qualified Data.Aeson.Parser

data Login = Login
  { email        :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Login
instance FromJSON Login

type API = "login" :> ReqBody '[JSON] Login :> Post '[JSON] [String]

startApp :: IO ()
startApp = do
    h <- R.connect "172.17.0.2" 28015 Nothing
    users <- RNC.run h $ RNC.table "users" :: IO (RNC.Cursor RNC.Datum)
    c <- RNC.collect users
    putStrLn $ show c
    run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

postLogin _ = do
    return ["received"]

server :: Server API
server = postLogin
