module YTCasts.Exe where

import Prelude
import Control.Monad.Aff (Canceler, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (for_, traverse_)
import Data.Traversable (for)
import Network.HTTP.Affjax (AJAX)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import SQLite3 (DBEffects, closeDB, newDB)
import YTCasts (CastStatus(CastDownloaded, CastAlreadyDownloaded), downloadCasts, parseConfig)

type Program e =
  ( ajax :: AJAX
  , console :: CONSOLE
  , fs :: FS
  , db :: DBEffects
  | e
  )

mainExe :: forall e.
  Eff
    (Program (err :: EXCEPTION | e))
    (Canceler (Program e))
mainExe = launchAff do
  config <- parseConfig <$> readTextFile UTF8 "./config.json"
  conn <- newDB "./data"
  for_ config.targets $ downloadCasts conn
  closeDB conn
