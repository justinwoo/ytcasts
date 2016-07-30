module Main where

import Prelude
import Network.HTTP.Affjax as Affjax
import Control.Monad.Aff (makeAff, Aff, Canceler, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (length)
import Data.Foldable (for_, traverse_)
import Data.Function.Uncurried (runFn4, runFn2, Fn4, Fn2)
import Data.Traversable (for)
import Network.HTTP.Affjax (AffjaxResponse, AJAX, URL)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)

foreign import parseConfig :: String -> Config

type HTMLString = String

foreign import getCasts :: HTMLString -> Array Cast

type Query = String

type Param = String

foreign import data DBRow :: *
foreign import data DBConnection :: *
foreign import data DBEffects :: !

foreign import _newDB :: forall e.
  Fn2
    FilePath
    (DBConnection -> Eff (db :: DBEffects | e) Unit)
  (Eff (db :: DBEffects | e) Unit)
foreign import _closeDB :: forall e.
  Fn2
    DBConnection
    (Unit -> Eff (db :: DBEffects | e) Unit)
    (Eff (db :: DBEffects | e) Unit)
foreign import _queryDB :: forall e.
  Fn4
    DBConnection
    Query
    (Array Param)
    (Array DBRow -> Eff (db :: DBEffects | e) Unit)
  (Eff (db :: DBEffects | e) Unit)
foreign import _runDownload :: forall e.
  Fn2
    URL
    (Unit -> Eff (fs :: FS | e) Unit)
  (Eff (fs :: FS | e) Unit)

foreign import parseRow :: DBRow -> CastRecord

newDB :: forall e. FilePath -> Aff (db :: DBEffects | e) DBConnection
newDB path = makeAff (\e s -> runFn2 _newDB path s)
closeDB :: forall e. DBConnection -> Aff (db :: DBEffects | e) Unit
closeDB conn = makeAff (\e s -> runFn2 _closeDB conn s)
queryDB :: forall e. DBConnection -> Query -> Array Param -> Aff (db :: DBEffects | e) (Array DBRow)
queryDB conn query params = makeAff (\e s -> runFn4 _queryDB conn query params s)
runDownload :: forall e. URL -> Aff (fs :: FS | e) Unit
runDownload url = makeAff (\e s -> runFn2 _runDownload url s)

type Program e =
  ( ajax :: AJAX
  , console :: CONSOLE
  , fs :: FS
  , db :: DBEffects
  | e
  )

type Config =
  { targets :: Array URL }

type Cast =
  { title :: String
  , link :: URL
  }

type CastRecord =
  { title :: String
  , link :: String
  , path :: String
  , created :: String
  }

data CastStatus
  = CastAlreadyDownloaded
  | CastDownloaded Cast

downloadCast :: forall e. DBConnection -> Cast -> Aff (db :: DBEffects, fs :: FS | e) CastStatus
downloadCast conn cast = do
  exists <- (\rows -> 1 == length rows) <$> queryDB conn "SELECT 1 from downloads where link = ?" [cast.link]
  case exists of
    true -> pure CastAlreadyDownloaded
    false -> do
      runDownload cast.link
      queryDB conn "INSERT INTO downloads (link, title, created) VALUES ($1, $2, datetime('now'));" [cast.link, cast.title]
      pure $ CastDownloaded cast

downloadCasts :: forall e. DBConnection -> String -> Aff (ajax :: AJAX, fs :: FS, db :: DBEffects | e) (Array CastStatus)
downloadCasts conn url = do
  res :: AffjaxResponse String <- Affjax.get url
  let casts = getCasts res.response
  for casts $ downloadCast conn

main :: forall e.
  Eff
    (Program (err :: EXCEPTION | e))
    (Canceler (Program e))
main = launchAff do
  config <- parseConfig <$> readTextFile UTF8 "./config.json"
  conn <- newDB "./data"
  targetStatuses :: Array (Array CastStatus) <- for config.targets $ downloadCasts conn
  for_ targetStatuses $ reportTargetStatus
  closeDB conn
  where
    reportTargetStatus =
      traverse_ reportStatus
    reportStatus status =
      case status of
        CastAlreadyDownloaded -> pure unit
        CastDownloaded cast -> log $ "downloaded " <> cast.title <> " from " <> cast.link
