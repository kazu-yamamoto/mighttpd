{-# LANGUAGE OverloadedStrings #-}

module File (mighty, progName) where

import Control.Applicative
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import Network.TCPInfo
import Network.Web.Server
import Network.Web.Server.Basic
import Network.Web.URI
import System.Directory
import System.FilePath
import System.IO
import System.Posix.Files
import URLMap

progName :: String
progName = "Mighttpd"

progVersion :: String
progVersion = "0.4.2"

progNameVersion :: String
progNameVersion = progName ++ "/" ++ progVersion

----------------------------------------------------------------

mighty :: WebConfig -> URLMap -> Handle -> TCPInfo -> IO ()
mighty wcnf umap hdl tcpinfo = do
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
    | from `isPrefixOf` url = toPath to $ drop (length from) url
    | otherwise             = lookupFileMap xs url

toPath :: ConvInfo -> FilePath -> Path
toPath (CIFile dir)   restPath  = File $ dir </> restPath
toPath ci@(CICgi _ _) progParam = PathCGI CGI {
    progPath    = prog
    , scriptName  = scriptname
    , pathInfo    = path
    , queryString = query
    }
  where
    (progParam',query)  = break (== '?') progParam
    (prog',path)        = break (== '/') progParam'
    prog = progDir ci </> prog'
    scriptname = pathInURL ci </> prog'

fileMapper :: URLMap -> URI -> Path
fileMapper umap uri = fileMapper' (lookupFileMap umap url)
  where
    url = unEscapeString . S.unpack . toURLwoPort $ uri
    fileMapper' None                  = None
    fileMapper' cgi@(PathCGI _)       = cgi
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
