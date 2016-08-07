module Main where

import YTCasts.Exe
import Control.Monad.Aff (Canceler)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.HTTP.Affjax (AJAX)
import Node.FS (FS)
import SQLite3 (DBEffects)

main :: forall e.
  Eff
    ( ajax :: AJAX
    , console :: CONSOLE
    , fs :: FS
    , db :: DBEffects
    , err :: EXCEPTION
    | e
    )
    (Canceler
       ( ajax :: AJAX
       , console :: CONSOLE
       , fs :: FS
       , db :: DBEffects
       | e
       )
    )
main = mainExe
