{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( bsToChr
    , newFS
    , retrieveUrl
    , URL
    , FileSystem
    ) where

import Control.Monad.Trans.Resource (runResourceT)
import Crypto.Hash.MD5 (hash)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Lazy as L
import Data.Char (chr)
import Data.Conduit.Binary (sinkFile)
import Data.Conduit
import Data.String (fromString)
import Network.HTTP.Conduit
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))
import System.Posix.Files (fileExist)

type URL = String
data FileSystem = FileSystem { directory :: FilePath
                             }
                             deriving (Show, Eq)

newFS :: FilePath -> FileSystem
newFS = FileSystem

-- Downloads a given url to the provided path
downloadFile :: URL -> FilePath -> IO ()
downloadFile url file = do
  request <- parseUrl (url :: String)
  manager <- newManager tlsManagerSettings
  runResourceT $ do
         response <- http request manager
         responseBody response $$+- sinkFile file

retrieveUrl :: FileSystem -> URL -> IO (Maybe L.ByteString)
retrieveUrl fs url = do
    exists <- fileExist path
    contents <- if exists 
                then L.readFile path
                else retUrl
    return $ Just contents
  where
    (rd, path) = makeFilePath fs url
    retUrl     = do
        createDirectoryIfMissing True rd
        downloadFile url path
        L.readFile path

-- Builds the 
makeFilePath :: FileSystem -> String -> (FilePath, FilePath)
makeFilePath (FileSystem dir) url = (rootdir, rootdir </> hashed)
  where
    hashed   = bsToChr . encode . hash $ C8.pack url
    (d1, xs) = splitAt 2 hashed
    d2       = take 2 xs
    rootdir  = dir </> d1 </> d2

bsToChr :: B.ByteString -> String
bsToChr  = map (chr . fromEnum) . B.unpack
