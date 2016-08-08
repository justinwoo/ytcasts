{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson
import Control.Monad.IO.Class
import GHC.Generics
import Servant
import System.Directory
import System.Environment
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Static
import System.IO

data Cast = Cast
  { castTitle :: String
  , castPath :: FilePath
  }
  deriving (Show, Eq, Generic)

instance ToJSON Cast
instance FromJSON Cast

type CastAPI
  = "casts" :> Get '[JSON] [Cast]

castApi :: Proxy CastAPI
castApi = Proxy

data AppContext = AppContext { contextPath :: FilePath }

getServer :: AppContext -> Server CastAPI
getServer =
  getCasts

getCasts :: AppContext -> Handler [Cast]
getCasts ctx =
  liftIO $ getCastDirList $ contextPath ctx

getCastDirList :: FilePath -> IO [Cast]
getCastDirList path = do
  files <- getDirectoryContents path
  pure $ makeCast =<< files
  where
    makeCast :: FilePath -> [Cast]
    makeCast x
      | x `elem` [ "..", ".", ".gitkeep" ] = mempty
      | otherwise = pure $ Cast x ("/" ++ x)

main :: IO ()
main = do
  path <- getEnv "YTCASTS_HOME"
  let ctx = AppContext path
      port = 3000
      settings =
          setPort port $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
          defaultSettings
      server = getServer ctx
      app
        = staticPolicy (addBase path)
        . simpleCors
        $ serve castApi server
  runSettings settings app
