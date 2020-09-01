{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    , app
    ) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.Verbs
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad.IO.Class

import Types
import BreakingBadQuoteReq

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = UserAPI :<|> OtherAPI :<|> StaticAPI :<|> BreakingBadAPI

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy


server :: Server API
server = userServer :<|> otherServer :<|> staticServer :<|> breakingBadServer

type BreakingBadAPI = "breaking-bad" :> "quote" :> Get '[JSON] BreakingBadQuote

breakingBadServer :: Server BreakingBadAPI
breakingBadServer = do
    quote <- liftIO randomQuote
    return quote

type StaticAPI = "static" :> Raw

staticServer :: Server StaticAPI
staticServer = serveDirectoryWebApp "static"

type OtherAPI = "empty" :> EmptyAPI

otherServer :: Server OtherAPI
otherServer = emptyServer

type UserAPI = "users" :>
    (    Get '[JSON] [User]
    :<|> "albert" :> Get '[JSON] User
    :<|> "isaac" :> Get '[JSON] User
    :<|> "list-all" :> QueryParam "sortby" String :> Get '[JSON] [User]
    :<|> Capture "userid" Integer :> Get '[JSON] User
    )
   -- :<|> "users" :> Capture "userid" Integer :> DeleteNoContent

userServer :: Server UserAPI
userServer = return users
      :<|> return albert
      :<|> return isaac
      :<|> listWithSort
      :<|> userById
     -- :<|> removeUser

  where listWithSort :: Maybe String -> Handler [User]
        listWithSort (Just x) = return $ sortBy (comparing userFirstName) users
        listWithSort Nothing = return users

        userById :: Integer -> Handler User
        userById _ = return albert

        removeUser :: Int -> Handler NoContent
        removeUser _ = undefined

isaac = User 1 "Isaac" "Newton"
albert = User 2 "Albert" "Einstein"

users :: [User]
users = [ isaac
        , albert
        ]
