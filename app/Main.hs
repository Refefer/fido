{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.String (IsString)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.String (fromString)
import Network.Wai (responseLBS, responseFile, rawPathInfo, Application, Response)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Options.Applicative

import Lib

data Settings = Settings { port :: Int
                         , path :: String 
                         }

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

app :: FileSystem -> Application
app fs req respond = do
    let p = bsToChr . B.tail $ rawPathInfo req 
    resp <- dispatch p
    respond resp
  where
    dispatch "ping" = ping
    dispatch path = download fs path

-- Parser options via applicative
settings :: Parser Settings 
settings = Settings 
  <$> option auto
    ( long "port"
   <> short 'p'
   <> help "Port to run fido on" )
  <*> strOption
    ( long "path"
   <> short 'd' 
   <> metavar "DIRECTORY" 
   <> help "Path to the directory for storing the files" )

main = do
    s <- execParser opts
    let p = port s
    putStrLn $ "Listening on port " ++ show p
    let fs = newFS . path $s
    run p (app fs)
  where
    opts = info (helper <*> settings)
      ( fullDesc
     <> progDesc "Runs a server that proxies and caches images to disk"
     <> header "fido - fetches files from a server" )

