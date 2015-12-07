{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import Data.String (fromString)
import Network.Wai (responseLBS, rawPathInfo, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Lib

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    let fs = newFS "/tmp/cached"
    run port (app fs)

app :: FileSystem -> Application
app fs req respond = do
    let p = bsToChr . B.tail $ rawPathInfo req 
    inf <- retrieveUrl fs p
    respond $ case inf of
      Just lbs -> responseLBS status200 [(hContentType, "image/jpeg")] lbs 
      Nothing  -> responseLBS status404 [(hContentType, "text/plain")] "404 - Not Found" 

