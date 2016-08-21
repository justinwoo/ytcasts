{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.List
import Data.Swagger hiding (port)
import Data.Time.Clock
import Data.Typeable
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Static
import Servant
import Servant.Swagger
import System.Directory
import System.Environment
import System.IO

data Cast = Cast
  { castTitle :: String
  , castPath :: FilePath
  , castTime :: UTCTime
  }
  deriving (Show, Eq, Generic, Typeable)

instance ToJSON Cast
instance FromJSON Cast
instance ToSchema Cast

type CastAPI
  = "casts" :> Get '[JSON] [Cast]

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type API = SwaggerAPI :<|> CastAPI

api :: Proxy API
api = Proxy

castSwagger :: Swagger
castSwagger = toSwagger castApi
  & info.title .~ "Cast API"
  & info.version .~ "1.0"
  & info.description ?~ "YTCasts API"

castApi :: Proxy CastAPI
castApi = Proxy

data AppContext = AppContext { contextPath :: FilePath }

getServer :: AppContext -> Server CastAPI
getServer = getCasts

getCasts :: AppContext -> Handler [Cast]
getCasts ctx =
  liftIO $ getCastDirList $ contextPath ctx

getCastDirList :: FilePath -> IO [Cast]
getCastDirList path = do
  files <- filter filterNonFiles <$> getDirectoryContents path
  casts <- makeCast `traverse` files
  pure $ reverse $ sortOn castTime casts
  where
    makeCast :: FilePath -> IO Cast
    makeCast x = do
      let castPath = "/" ++ x
      castTime <- getModificationTime (path ++ castPath)
      pure $ Cast x castPath castTime

    filterNonFiles :: FilePath -> Bool
    filterNonFiles x = x `notElem` [ "..", ".", ".gitkeep" ]

server :: FilePath -> IO (Server API)
server path = do
  let ctx = AppContext path
  pure $
    pure castSwagger
    :<|> getServer ctx

app :: IO Application
app = do
  path <- getEnv "YTCASTS_HOME"
  staticPolicy (addBase path)
    . simpleCors
    . serve api <$> server path

main :: IO ()
main = do
  let port = 3000
      settings =
          setPort port $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
          defaultSettings
  runSettings settings =<< app
