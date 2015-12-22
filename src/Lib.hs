{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
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
import Data.Conduit.Binary (sourceFile, sinkFile, sinkLbs)
import Data.Conduit
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.String (fromString)
import Network.HTTP.Conduit
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath.Posix ((</>), splitFileName)
import System.Posix.Files (fileExist)
import System.Random (randomIO)

type URL = String
data FileSystem = FileSystem { directory  :: FilePath
                             , urlfrag    :: String
                             , shared_cache :: Maybe FilePath
                             , cache_shared_local :: Bool
                             }
                             deriving (Show, Eq)

newtype App a = App { unApp :: ReaderT FileSystem IO a 
                    } deriving (Functor
                               , Applicative
                               , Monad
                               , MonadIO
                               , MonadReader FileSystem
                               )

retrieveUrl :: FileSystem -> URL -> IO (Maybe FilePath)
retrieveUrl fs url = runReaderT (unApp $ retUrl url) fs

retUrl :: URL -> App (Maybe FilePath)
retUrl url = do
    let (dirname, basename) = hashRelPath url
    let relPath = dirname </> basename
    mCached <- checkCaches relPath
    case mCached of
      Just path -> return $ Just path
      Nothing -> do
          cdir <- asks directory
          let abspath = cdir </> relPath
          liftIO $ createDirectoryIfMissing True (cdir </> dirname) *>
                   downloadFile url abspath

-- Checks first the local cache then the shared cache
checkCaches :: FilePath -> App (Maybe FilePath)
checkCaches relPath = do
  dir   <- asks directory
  rcm   <- asks shared_cache
  share <- asks cache_shared_local
  -- Not a good way to do this
  liftIO $ do
    cd1 <- check dir
    case cd1 of
      Just x  -> return $ Just x
      Nothing -> case rcm of
                  Just rc -> (if share then (checkCopy dir) else check) rc
                  _       -> return Nothing
  
 where
  path base = base </> relPath
  check :: FilePath -> IO (Maybe FilePath)
  check base = do
    let abs_path = path base
    exists <- fileExist abs_path 
    return $ if exists then (Just abs_path) else Nothing

  checkCopy local base = do
    res <- check base
    case res of
      Just p -> atomicCopy p (path local)
      _      -> return ()

    return res

atomicCopy :: FilePath -> FilePath -> IO ()
atomicCopy from to = do
  id <- randomIO :: IO Int 
  let baseDir = fst . splitFileName $ to
  let tmpPath = baseDir </> ("." ++ show id)
  copyFile tmpPath from
  renameFile tmpPath to
 
copyFile :: FilePath -> FilePath -> IO ()
copyFile to from = do
  createDirectoryIfMissing True (fst . splitFileName $ to)
  runResourceT $ sourceFile from $$ sinkFile to

hashRelPath :: URL -> (FilePath, FilePath)
hashRelPath url = (dirname, hashed)
  where
    frag     = drop 2 . dropWhile (/= '/') $ url
    hashed   = bsToChr . encode . hash $ C8.pack frag
    (d1, xs) = splitAt 2 hashed
    d2       = take 2 xs
    dirname  = d1 </> d2 

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

