module Main where

import Prelude

import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Console (errorShow, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (find, head, length, snoc)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), contains, split, trim)
import Data.String.HtmlElements (decode)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import LenientHtmlParser (Attribute(..), Name(..), Tag(..), TagName(..), Value(..), parseTags)
import Milkis as M
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import SQLite3 (DBConnection, DBEffects, closeDB, newDB, queryDB)
import Simple.JSON (class ReadForeign, readJSON)
import Sunde as Sunde
import Text.Parsing.StringParser (ParseError)
import Unsafe.Coerce (unsafeCoerce)

newtype Url = Url String
derive instance newtypeUrl :: Newtype Url _
derive newtype instance readForeignUrl :: ReadForeign Url

type HTMLString = String

type Query = String

type Param = String

runDownload :: forall e.
  Url ->
  Aff
    ( cp :: CP.CHILD_PROCESS
    , exception :: EXCEPTION
    , ref :: REF
    | e
    )
    (Either String String)
runDownload (Url url) = do
  result <- Sunde.spawn
    "youtube-dl"
    [ "-o"
    , "downloads/%(title)s.%(ext)s"
    , "-x"
    , "--audio-format"
    , "mp3"
    , url
    ]
    $ CP.defaultSpawnOptions
        { stdio = [Just CP.Pipe]
        }
  pure case result.exit of
    CP.Normally 0 -> Right "success?"
    _ -> Left result.stderr

type Config =
  { targets :: Array Url }

type Cast =
  { title :: String
  , link :: Url
  }

data CastStatus
  = CastAlreadyDownloaded
  | CastDownloaded Cast
  | CastDownloadFailed String Cast

downloadCast ::
  forall e.
  DBConnection ->
  Cast ->
  Aff
    (Program e)
    CastStatus
downloadCast conn cast = do
  exists <- (\rows -> 1 == length (unsafeCoerce rows)) <$> queryDB conn "SELECT 1 from downloads where link = ?" [unwrap cast.link]
  case exists of
    true -> pure CastAlreadyDownloaded
    false -> do
      let info = cast.title <> " from " <> unwrap cast.link
      log $ "downloading: " <> info
      result <- runDownload cast.link
      case result of
        Right _ -> do
          _ <- queryDB conn "INSERT INTO downloads (link, title, created) VALUES ($1, $2, datetime('now'));" [unwrap cast.link, cast.title]
          log $ "  downloaded: " <> info
          pure $ CastDownloaded cast
        Left e -> do
          log $ "cast download failed of " <> cast.title <> " " <> show e
          pure $ CastDownloadFailed e cast

getCasts :: HTMLString -> Either ParseError (Array Cast)
getCasts s = do
  tags <- parseTags s
  pure $ tailRec getLinks (Tuple mempty tags)
  where
    getLinks (Tuple acc (TagOpen (TagName "a") attrs : TNode tnode : TagClose (TagName "a") : xs))
      | Just true <- contains (Pattern "yt-uix-tile-link") <$> (getAttr "class" attrs)
      , title <- trim tnode
      , Just (Just href) <- head <<< split (Pattern "&") <$> getAttr "href" attrs
      , link <- Url $ "https://www.youtube.com" <> href = Loop (Tuple (snoc acc {title, link}) xs)
      | otherwise = Loop (Tuple acc xs)
    getLinks (Tuple acc (_ : xs)) = Loop (Tuple acc xs)
    getLinks (Tuple acc _) = Done acc
    getAttr match xs = getValue <$> find matchName xs
      where
        matchName (Attribute (Name name) _) = match == name
        getValue (Attribute _ (Value x)) = decode <<< trim $ x

downloadCasts ::
  forall e.
  DBConnection ->
  Url ->
  Aff
    (Program e)
    (Array CastStatus)
downloadCasts conn (Url url) = do
  res <- M.text =<< M.fetch (M.URL url) M.defaultFetchOptions
  case getCasts res of
    Right casts -> for casts $ downloadCast conn
    Left e -> do
      errorShow e
      pure []

type Program e =
  ( console :: CONSOLE
  , cp :: CP.CHILD_PROCESS
  , ref :: REF
  , exception :: EXCEPTION
  , fs :: FS
  , db :: DBEffects
  | e
  )

main :: forall e.
  Eff
    (Program (exception :: EXCEPTION | e))
    Unit
main = launchAff_ do
  decoded <- readJSON <$> readTextFile UTF8 "./config.json"
  case decoded of
    Right (config :: Config) -> do
      conn <- newDB "./data"
      _ <- queryDB conn "CREATE TABLE IF NOT EXISTS downloads (link varchar(20) primary key unique, title varchar, created datetime);" []
      for_ config.targets $ (downloadCasts conn)
      closeDB conn
    Left e -> do
      errorShow e
