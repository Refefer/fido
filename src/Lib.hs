{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib
    ( bsToChr
    , retrieveUrl
    , URL
    , FileSystem(..)
    ) where

import Control.Monad.Reader
import Control.Monad.Trans (liftIO, lift)
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
                             , urlfrag   :: String
                             }
                             deriving (Show, Eq)

newtype App a = App { unApp :: ReaderT FileSystem IO a 
                    } deriving (Functor
                               , Applicative
                               , Monad
                               , MonadIO
                               , MonadReader FileSystem
                               , MonadTrans
                               )

retrieveUrl :: FileSystem -> URL -> IO (Maybe FilePath)
retrieveUrl fs url = runReaderT (unApp $ retUrl url) fs

retUrl :: URL -> App (Maybe FilePath)
retUrl url = do
    (rd, path) <- makeFilePath url
    exists <- liftIO $ fileExist path
    path   <- if exists 
              then lift $ Just path
              else do
        liftIO $ createDirectoryIfMissing True rd *> downloadFile url path
    return path

-- Builds the 
makeFilePath :: URL -> App (FilePath, FilePath)
makeFilePath url = do
  dir <- asks directory
  let hashed   = bsToChr . encode . hash $ C8.pack url
  let (d1, xs) = splitAt 2 hashed
  let d2       = take 2 xs
  let rootdir  = dir </> d1 </> d2
  return (rootdir, rootdir </> hashed)

bsToChr :: B.ByteString -> String
bsToChr  = map (chr . fromEnum) . B.unpack

-- Downloads a given url to the provided path
downloadFile :: URL -> FilePath -> IO (Maybe FilePath)
downloadFile url file = do
    request <- parseUrl (url :: String)
    manager <- newManager tlsManagerSettings
    run $ do
      response <- http request manager
      responseBody response $$+- sinkFile file
      return $ Just file

  where
    run f = (runResourceT f) `catch` handleExc

handleExc :: HttpException -> IO (Maybe FilePath)
handleExc _ = return Nothing

