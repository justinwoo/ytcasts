module Main where

import Prelude
import Network.HTTP.Affjax as Affjax
import Control.Monad.Aff (Aff, Canceler, launchAff, makeAff)
import Control.Monad.Aff.Console (errorShow, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Except (runExcept)
import Data.Array (find, foldMap, length)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.String (Pattern(..), contains, trim)
import Data.String.HtmlElements (decode)
import Data.Traversable (for)
import LenientHtmlParser (Attribute(..), Name(..), Tag(..), TagName(..), Value(..), parseTags)
import Network.HTTP.Affjax (AJAX, URL)
import Node.ChildProcess (CHILD_PROCESS, StdIOBehaviour(..), defaultSpawnOptions, onError, onExit, spawn, toStandardError)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import SQLite3 (DBConnection, DBEffects, closeDB, newDB, queryDB)
import Text.Parsing.StringParser (ParseError)
import Unsafe.Coerce (unsafeCoerce)

type HTMLString = String

type Query = String

type Param = String

runDownload :: forall e.
  URL ->
  Aff
    ( cp :: CHILD_PROCESS
    | e
    )
    (Either Error String)
runDownload url = makeAff \e s -> do
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
  { targets :: Array URL }
derive instance genericConfig :: Generic Config _
instance decodeConfig :: Decode Config where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

type Cast =
  { title :: String
  , link :: URL
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
  exists <- (\rows -> 1 == length (unsafeCoerce rows)) <$> queryDB conn "SELECT 1 from downloads where link = ?" [cast.link]
  case exists of
    true -> pure CastAlreadyDownloaded
    false -> do
      result <- runDownload cast.link
      case result of
        Right _ -> do
          _ <- queryDB conn "INSERT INTO downloads (link, title, created) VALUES ($1, $2, datetime('now'));" [cast.link, cast.title]
          log $ "downloaded " <> cast.title <> " from " <> cast.link
          pure $ CastDownloaded cast
        Left e -> do
          log $ "cast download failed of " <> cast.title <> " " <> show e
          pure $ CastDownloadFailed e cast

getCasts :: HTMLString -> Either ParseError (Array Cast)
getCasts s = do
  tags <- parseTags s
  pure $ foldMap getLinks tags
  where
    getLinks (TagOpen (TagName "a") attrs) = do
      case contains (Pattern "yt-uix-tile-link") <$> (getAttr "class" attrs) of
        Just true -> do
          case {title: _, link: _}
            <$> getAttr "title" attrs
            <*> ((<>) "https://www.youtube.com" <$> getAttr "href" attrs)
            of
            Just a -> pure a
            Nothing -> mempty
        _ -> mempty
    getLinks _ = mempty
    getAttr match xs = getValue <$> find matchName xs
      where
        matchName (Attribute (Name name) _) = match == name
        getValue (Attribute _ (Value x)) = decode <<< trim $ x

downloadCasts ::
  forall e.
  DBConnection ->
  String ->
  Aff
    (Program e)
    (Array CastStatus)
downloadCasts conn url = do
  res <- Affjax.get url
  case getCasts res.response of
    Right casts -> for casts $ downloadCast conn
    Left e -> do
      errorShow e
      pure []

type Program e =
  ( ajax :: AJAX
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
      for_ config.targets $ (downloadCasts conn)
      closeDB conn
    Left e -> do
      errorShow e
