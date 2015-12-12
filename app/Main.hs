{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.String (IsString)
import Data.Maybe (fromMaybe)
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

readCache :: ReadM FileSystem
readCache = eitherReader $ \x -> do
    case (split x) of
      Just (left, right) -> Right $ FileSystem left right
      _                  -> Left "Format is /path/on/disk=>url"
  where
    index [] _ = Nothing
    index ('=':'>':_) i = Just (i + 1)
    index (_:xs) i = index xs (i + 1)
    split :: String -> Maybe (String, String)
    split hs = do
      idx <- index hs 0
      return (take (idx - 1) hs, drop (idx + 1) hs)

data Settings = Settings { port :: Int
                         , caches :: [FileSystem]
                         } deriving (Show, Eq)

textPlain = [(hContentType, "text/plain")]
imageJpeg = [(hContentType, "image/jpeg")]

plain :: L.ByteString -> Response
plain a = responseLBS status200 textPlain a

ping :: IO Response
ping = return $ plain "pong"

download :: FileSystem -> URL -> IO Response
download fs path = do
  inf <- retrieveUrl fs path
  return $ case inf of
    Just path -> responseFile status200 imageJpeg path Nothing
    Nothing   -> responseLBS status404 textPlain "404 - Not Found" 

app :: [FileSystem] -> Application
app fss req respond = do
    let p = bsToChr . B.tail $ rawPathInfo req 
    let (ns, path) = break (== '/') p
    resp <- dispatch ns (tail path)
    respond resp
  where
    dispatch "ping" _ = ping
    dispatch ns path = download (lookupFS ns) path
    urlMap = Map.fromList $ fmap (\fs -> (urlfrag fs, fs)) fss
    lookupFS n = fromMaybe (head fss) $ Map.lookup n urlMap

-- Parser options via applicative
settings :: Parser Settings 
settings = Settings 
  <$> option auto
    ( long "port"
   <> short 'p'
   <> help "Port to run fido on" )
  <*> many (argument readCache
    ( metavar "CACHES" 
   <> help "Path to the directory for storing the files" ))

main = do
    s <- execParser opts
    let p = port s
    putStrLn $ "Listening on port " ++ show p
    run p (app (caches s))
  where
    opts = info (helper <*> settings)
      ( fullDesc
     <> progDesc "Runs a server that proxies and caches images to disk"
     <> header "fido - fetches files from a server" )

