module YTCasts where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (and, find)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), contains, split, trim)
import Data.String.HtmlElements (decode)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (errorShow, log)
import LenientHtmlParser (Attribute(..), Name(..), Tag(..), TagName(..), Value(..), parseTags)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import SQLite3 (DBConnection, closeDB, newDB, queryDB)
import Sunde as Sunde
import Text.Parsing.StringParser (ParseError)
import Tortellini (parsellIni)
import Unsafe.Coerce (unsafeCoerce)

newtype Url = Url String
derive instance newtypeUrl :: Newtype Url _

type HTMLString = String

type Query = String

type Param = String

runDownload :: Url -> Aff (Either String String)
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
  { ytcasts ::
      { channels :: Array String
      , limit :: Int
      }
  }

type Cast =
  { title :: String
  , link :: Url
  }

data CastStatus
  = CastAlreadyDownloaded
  | CastDownloaded Cast
  | CastDownloadFailed String Cast

downloadCast :: DBConnection -> Cast -> Aff CastStatus
downloadCast conn cast = do
  exists <- (\rows -> 1 == Array.length (unsafeCoerce rows)) <$> queryDB conn "SELECT 1 from downloads where link = ?" [unwrap cast.link]
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

getCasts :: HTMLString -> Either ParseError (List Cast)
getCasts s = do
  tags <- parseTags s
  pure $ tailRec getLinks (Tuple mempty tags)
  where
    getLinks (Tuple acc (TagOpen (TagName "a") attrs : TNode tnode : TagClose (TagName "a") : xs))
      | Just true <- contains (Pattern "yt-uix-tile-link") <$> (getAttr "class" attrs)
      , title <- trim tnode
      , Just (Just href) <- Array.head <<< split (Pattern "&") <$> getAttr "href" attrs
      , link <- Url $ "https://www.youtube.com" <> href = Loop (Tuple (List.Cons {title, link} acc) xs)
      | otherwise = Loop (Tuple acc xs)
    getLinks (Tuple acc (_ : xs)) = Loop (Tuple acc xs)
    getLinks (Tuple acc _) = Done acc
    getAttr match xs = getValue <$> find matchName xs
      where
        matchName (Attribute (Name name) _) = match == name
        getValue (Attribute _ (Value x)) = decode <<< trim $ x

fetchCasts :: Url -> Aff (List Cast)
fetchCasts (Url url) = do
  res <- M.text =<< M.fetch nodeFetch (M.URL url) M.defaultFetchOptions
  case getCasts res of
    Right casts ->
      pure casts
    Left e -> do
      errorShow e
      pure mempty

ensureDB :: DBConnection -> Aff Unit
ensureDB conn =
  void $ queryDB conn """
CREATE TABLE IF NOT EXISTS downloads
(link varchar(20) primary key unique, title varchar, created datetime);
""" []

prepareURL :: String -> Url
prepareURL channel =
  Url $ "https://www.youtube.com/user/" <> channel <> "/videos"

main :: Effect Unit
main = launchAff_ do
  decoded <- parsellIni <$> readTextFile UTF8 "./config.ini"
  case decoded of
    Right (config :: Config) -> do
      bracket (newDB "./data") (liftEffect <<< closeDB) (withConn config.ytcasts)
    Left e -> do
      errorShow e
  where
    withConn config conn = do
      ensureDB conn
      log "Fetching targets..."
      let targets = List.fromFoldable $ prepareURL <$> config.channels
      casts <- merge <$> traverse fetchCasts targets
      log $ "Found " <> show (List.length casts) <> " targets."
      log $ "Using limit " <> show config.limit <> "."
      traverse_ (downloadCast conn) (List.take config.limit casts)
      log $ "Done."

    -- merge these damn lists
    merge :: List (List Cast) -> List Cast
    merge xs = iter mempty xs

    -- what a mess
    iter :: List Cast -> List (List Cast) -> List Cast
    iter xs ys =
      case and (List.null <$> ys) of
        true -> xs
        _ ->
          let
            heads :: List Cast
            heads = List.catMaybes $ List.head <$> ys
            tails :: List (List Cast)
            tails = (fromMaybe mempty <<< List.tail) <$> ys
          in
          iter (heads <> xs) tails
