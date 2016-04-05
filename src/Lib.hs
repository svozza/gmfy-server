{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Servant
import Prelude hiding (id)
import qualified Data.Aeson.Parser

data Login = Login
  { email        :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Login
instance FromJSON Login

type API = "login" :> ReqBody '[JSON] Login :> Post '[JSON] Login

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = postLogin
  where postLogin = return
