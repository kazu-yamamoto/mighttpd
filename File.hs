{-# LANGUAGE OverloadedStrings #-}

module File (mighty, progName) where

import Control.Applicative
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import Network.Socket
import Network.Web.Server
import Network.Web.Server.Basic
import Network.Web.URI
import Network.Web.Utils
import System.Directory
import System.FilePath
import System.IO
import System.Posix.Files
import URLMap

progName :: String
progName = "Mighttpd"

progVersion :: String
progVersion = "0.3.0"

progNameVersion :: String
progNameVersion = progName ++ " " ++ progVersion

----------------------------------------------------------------

mighty :: WebConfig -> URLMap -> Socket -> IO ()
mighty wcnf umap s = do
  tcpinfo <- getTCPInfo s
  hdl <- socketToHandle s ReadWriteMode
  hSetBinaryMode hdl True
  let bcnf = BasicConfig { obtain = fileGet
                         , info   = fileInfo
                         , mapper = fileMapper umap
                         , serverName = S.pack progNameVersion
                         , tcpInfo = tcpinfo
                         }
  connection hdl (basicServer bcnf) wcnf

----------------------------------------------------------------

lookupFileMap :: URLMap -> URL -> Path
lookupFileMap [] _          = None
lookupFileMap ((from,to):xs) url
    | from `S.isPrefixOf` url = toPath to $ S.unpack $ S.drop (S.length from) url
    | otherwise               = lookupFileMap xs url
  where
    toPath (File dir) restPath = File $ dir </> restPath
    toPath (CGI dir _ urlPath) progParam = CGI prog (S.pack param) scriptName
      where
        (prog',param) = break (\x -> x == '?' || x == '/') progParam
        prog = dir </> prog'
        scriptName
          | "/" `S.isSuffixOf` urlPath = urlPath `S.append` S.pack prog'
          | otherwise                  = urlPath `S.append` "/" `S.append` S.pack prog'
    toPath _ _ = error "toPath"

fileMapper :: URLMap -> URI -> Path
fileMapper umap uri = fileMapper' (lookupFileMap umap url)
  where
    url = unEscapeString $ toURLwoPort uri
    fileMapper' None                  = None
    fileMapper' cgi@(CGI _ _ _)       = cgi
    fileMapper' (File file)
      | hasTrailingPathSeparator file = File $ file </> "index.html"
      | otherwise                     = File file

fileGet :: FilePath -> Maybe (Integer,Integer) -> IO L.ByteString
fileGet file Nothing = openFile file ReadMode >>= L.hGetContents
fileGet file (Just (skip,len)) = do
    h <- openFile file ReadMode
    hSeek h AbsoluteSeek skip
    L.take (fromIntegral len) <$> L.hGetContents h

fileInfo :: FilePath -> IO (Maybe (Integer, UTCTime))
fileInfo file = do
    exist <- doesFileExist file
    if exist
       then do
         fs <- getFileStatus file
         let size = fromIntegral . fileSize $ fs
             mtime = posixSecondsToUTCTime . realToFrac . modificationTime $ fs
         return $ Just (size, mtime)
       else return Nothing
