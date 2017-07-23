module Main where

import Prelude

import Control.Monad.Aff (Aff, Canceler, launchAff, makeAff)
import Control.Monad.Aff.Console (errorShow, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Except (runExcept)
import Data.Array (find, head, length, snoc)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode)
import Data.Generic.Rep (class Generic)
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), contains, split, trim)
import Data.String.HtmlElements (decode)
import Data.Traversable (for)
import LenientHtmlParser (Attribute(..), Name(..), Tag(..), TagName(..), Value(..), parseTags)
import Milkis (defaultFetchOptions, fetch, text)
import Node.ChildProcess (CHILD_PROCESS, StdIOBehaviour(..), defaultSpawnOptions, onError, onExit, spawn, toStandardError)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Node.HTTP (HTTP)
import SQLite3 (DBConnection, DBEffects, closeDB, newDB, queryDB)
import Text.Parsing.StringParser (ParseError)
import Unsafe.Coerce (unsafeCoerce)

newtype Url = Url String
derive instance newtypeUrl :: Newtype Url _
derive newtype instance decodeUrl :: Decode Url

type HTMLString = String

type Query = String

type Param = String

runDownload :: forall e.
  Url ->
  Aff
    ( cp :: CHILD_PROCESS
    | e
    )
    (Either Error String)
runDownload (Url url) = makeAff \e s -> do
  process <- spawn "youtube-dl"
             [ "-o"
             , "downloads/%(title)s.%(ext)s"
             , "-x"
             , "--audio-format"
             , "mp3"
             , url
             ]
             $ defaultSpawnOptions { stdio = [Just Pipe] }
  onError process $ toStandardError >>> Left >>> s
  onExit process $ const (s $ Right "success?")

newtype Config = Config
  { targets :: Array Url }
derive instance genericConfig :: Generic Config _
instance decodeConfig :: Decode Config where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

type Cast =
  { title :: String
  , link :: Url
  }

data CastStatus
  = CastAlreadyDownloaded
  | CastDownloaded Cast
  | CastDownloadFailed Error Cast

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
  pure $ getLinks mempty tags
  where
    getLinks acc (TagOpen (TagName "a") attrs : TNode tnode : TagClose (TagName "a") : xs)
      | Just true <- contains (Pattern "yt-uix-tile-link") <$> (getAttr "class" attrs)
      , title <- trim tnode
      , Just (Just href) <- head <<< split (Pattern "&") <$> getAttr "href" attrs
      , link <- Url $ "https://www.youtube.com" <> href = getLinks (snoc acc {title, link}) xs
      | otherwise = getLinks acc xs
    getLinks acc (_ : xs) = getLinks acc xs
    getLinks acc _ = acc
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
  res <- text =<< fetch url defaultFetchOptions
  case getCasts res of
    Right casts -> for casts $ downloadCast conn
    Left e -> do
      errorShow e
      pure []

type Program e =
  ( http :: HTTP
  , console :: CONSOLE
  , cp :: CHILD_PROCESS
  , fs :: FS
  , db :: DBEffects
  | e
  )

main :: forall e.
  Eff
    (Program (exception :: EXCEPTION | e))
    (Canceler (Program e))
main = launchAff do
  decoded <- decodeJSON <$> readTextFile UTF8 "./config.json"
  case runExcept decoded of
    Right (Config config) -> do
      conn <- newDB "./data"
      _ <- queryDB conn "CREATE TABLE IF NOT EXISTS downloads (link varchar(20) primary key unique, title varchar, created datetime);" []
      for_ config.targets $ (downloadCasts conn)
      closeDB conn
    Left e -> do
      errorShow e
