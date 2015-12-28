{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad (mapM_)
import Control.Monad.Reader
import Data.String (IsString)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.String (fromString)
import Network.Wai (responseLBS, responseFile, rawPathInfo, Application, Response)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Options.Applicative

import Lib

readCache :: ReadM Zone 
readCache = eitherReader $ \x -> do
    case (split x) of
      Just (left, right) -> Right $ Zone left right
      _                  -> Left "Format is /path/on/disk=>url"
  where
    index [] _ = Nothing
    index ('=':'>':_) i = Just (i + 1)
    index (_:xs) i = index xs (i + 1)
    split :: String -> Maybe (String, String)
    split hs = do
      idx <- index hs 0
      return (take (idx - 1) hs, drop (idx + 1) hs)

data Zone = Zone { path :: FilePath
                 , url :: String
                 } deriving (Show, Eq)

data Settings = Settings { port :: Int
                         , g_cache :: Maybe FilePath
                         , copyGlobal :: Bool
                         , verbose :: Bool
                         , zones :: [Zone]
                         } deriving (Show, Eq)

data AppSettings = AppSettings { fs :: FileSystem
                               , verboseLogging :: Bool 
                               }

newtype App a = App { unAR :: ReaderT AppSettings IO a 
                    } deriving (
                      Monad 
                    , MonadIO
                    , Applicative
                    , Functor
                    , MonadReader AppSettings
                    )

textPlain = [(hContentType, "text/plain")]
imageJpeg = [(hContentType, "image/jpeg")]

plain :: L.ByteString -> Response
plain a = responseLBS status200 textPlain a

ping :: IO Response
ping = return $ plain "pong"

ret404 = responseLBS status404 textPlain "404 - Not Found" 

download :: AppSettings -> URL -> IO Response
download as path = (flip runReaderT) as $ unAR $ do
  verb <- asks verboseLogging
  filesystem <- asks fs
  inf <- liftIO $ retrieveUrl filesystem verb path
  return $ case inf of
    Just path -> responseFile status200 imageJpeg path Nothing
    Nothing   -> ret404

app :: [FileSystem] -> Bool -> Application
app fss v req respond = do
    let p = bsToChr . B.tail $ rawPathInfo req 
    let (ns, path) = break (== '/') p
    resp <- dispatch p ns (tail path)
    respond resp
  where
    dispatch p ns path
      | p == "ping" = ping
      | isPrefixOf "http" p = downloadV (head fss) p
      | otherwise = case Map.lookup ns urlMap of
          Just fs -> downloadV fs path
          Nothing -> return ret404

    urlMap = Map.fromList $ fmap (\fs -> (urlfrag fs, fs)) fss
    downloadV fs = download (AppSettings fs v) 

-- Parser options via applicative
settings :: Parser Settings 
settings = Settings 
  <$> option auto
    ( long "port"
   <> short 'p'
   <> help "Port to run fido on" )
  <*> optional (strOption
    ( long "global-cache"
   <> short 'g'
   <> help "Optional global read cache for files" ))
  <*> switch
    ( long "global-to-local"
   <> short 'c'
   <> help "Copy global to local")
  <*> switch
    ( long "verbose"
   <> short 'v'
   <> help "Verbose logging")
  <*> many (argument readCache
    ( metavar "CACHES" 
   <> help "Caches in the format of /path/to/cache=>url_namespace" ))

main = do
    s <- execParser opts
    let p  = port s
    let gc = g_cache s
    let cg = copyGlobal s
    let v = verbose s

    let fss = [FileSystem p z gc cg | Zone p z <- zones s]
    putStrLn $ "Listening on port " ++ show p
    run p (app fss v)
  where
    opts = info (helper <*> settings)
      ( fullDesc
     <> progDesc "Runs a server that proxies and caches images to disk"
     <> header "fido - fetches files from a server" )

