{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( bsToChr
    , newFS
    , retrieveUrl
    , URL
    , FileSystem
    ) where

import Control.Exception (catch)
import Control.Monad.Trans.Resource (runResourceT)
import Crypto.Hash.MD5 (hash)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Lazy as L
import Data.Char (chr)
import Data.Conduit.Binary (sinkFile, sinkLbs)
import Data.Conduit
import Data.Maybe (maybeToList)
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
downloadFile :: URL -> FilePath -> IO (Maybe L.ByteString)
downloadFile url file = do
    request <- parseUrl (url :: String)
    manager <- newManager tlsManagerSettings
    resp <- run $ do
      response <- http request manager
      responseBody response $$+- fmap Just zss

    return $ fmap snd resp

    --return $ case resp of
    --  Left  e -> Nothing
    --  Right r -> Nothing --Just (fmap snd r)

  where
    zsf   = ZipSink (sinkFile file)
    zslbs = ZipSink sinkLbs
    zss   = getZipSink $ (\x y -> (x, y)) <$> zsf <*> zslbs
    run f = (runResourceT f) `catch` handleExc

handleExc :: HttpException -> IO (Maybe ((), L.ByteString))
handleExc _ = return Nothing

retrieveUrl :: FileSystem -> URL -> IO (Maybe L.ByteString)
retrieveUrl fs url = do
    exists   <- fileExist path
    contents <- if exists 
                then fmap Just (L.readFile path)
                else retUrl
    return contents
  where
    (rd, path) = makeFilePath fs url
    retUrl     = do
        createDirectoryIfMissing True rd
        downloadFile url path

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
