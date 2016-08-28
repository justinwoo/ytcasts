module YTCasts where

import Prelude
import Network.HTTP.Affjax as Affjax
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array (length)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Traversable (for)
import Network.HTTP.Affjax (AffjaxResponse, AJAX, URL)
import Node.FS (FS)
import SQLite3 (DBEffects, DBConnection, queryDB)

foreign import parseConfig :: String -> Config

type HTMLString = String

foreign import getCasts :: HTMLString -> Array Cast

type Query = String

type Param = String

foreign import _runDownload :: forall e.
  Fn2
    URL
    (Unit -> Eff (fs :: FS | e) Unit)
  (Eff (fs :: FS | e) Unit)

runDownload :: forall e. URL -> Aff (fs :: FS | e) Unit
runDownload url = makeAff (\e s -> runFn2 _runDownload url s)

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

reportStatus :: forall e. Cast -> Aff (console :: CONSOLE | e) Unit
reportStatus cast =
  log $ "downloaded " <> cast.title <> " from " <> cast.link

downloadCast ::
  forall e.
  DBConnection ->
  Cast ->
  Aff
    ( console :: CONSOLE
    , db :: DBEffects
    , fs :: FS
    | e
    )
    CastStatus
downloadCast conn cast = do
  exists <- (\rows -> 1 == length rows) <$> queryDB conn "SELECT 1 from downloads where link = ?" [cast.link]
  case exists of
    true -> pure CastAlreadyDownloaded
    false -> do
      runDownload cast.link
      queryDB conn "INSERT INTO downloads (link, title, created) VALUES ($1, $2, datetime('now'));" [cast.link, cast.title]
      reportStatus cast
      pure $ CastDownloaded cast

downloadCasts ::
  forall e.
  DBConnection ->
  String ->
  Aff
    ( ajax :: AJAX
    , console :: CONSOLE
    , db :: DBEffects
    , fs :: FS
    | e
    )
    (Array CastStatus)
downloadCasts conn url = do
  res :: AffjaxResponse String <- Affjax.get url
  let casts = getCasts res.response
  for casts $ downloadCast conn
