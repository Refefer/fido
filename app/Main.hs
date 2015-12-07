{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import Data.String (fromString)
import Network.Wai (responseLBS, rawPathInfo, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Options.Applicative

import Lib

data Settings = Settings { port :: Int
                         , path :: String 
                         }

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

app :: FileSystem -> Application
app fs req respond = do
    let p = bsToChr . B.tail $ rawPathInfo req 
    inf <- retrieveUrl fs p
    respond $ case inf of
      Just lbs -> responseLBS status200 [(hContentType, "image/jpeg")] lbs 
      Nothing  -> responseLBS status404 [(hContentType, "text/plain")] "404 - Not Found" 

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
     <> progDesc "fido - fetches files from a server" )

