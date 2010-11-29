{-# LANGUAGE OverloadedStrings #-}

module File (mighty, progName, fileMapper) where -- xxx

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

lookupFileMap :: URLMap -> URL -> Maybe (URL,ConvInfo)
lookupFileMap [] _          = Nothing
lookupFileMap (ent@(from,_):xs) url
    | from `isPrefixOf` url = Just ent
    | otherwise             = lookupFileMap xs url

fileMapper :: URLMap -> URI -> Path
fileMapper umap uri = case lookupFileMap umap url of
    Nothing           -> None
    Just (curl,cinfo) -> fileMapper' uri url curl cinfo
  where
    url = unEscapeString . S.unpack . toURLPath $ uri -- without param

fileMapper' :: URI -> URL -> URL -> ConvInfo -> Path
fileMapper' uri url curl cinfo = case cinfo of
    CIFile dir      -> toFile (dir </> path0)
    CICgi  dir path -> toCGI dir path
  where
    path0 = drop (length curl) url
    toFile path
      | hasTrailingPathSeparator path = File (path </> "index.html")
      | otherwise                     = File path
    toCGI dir path = PathCGI CGI {
        progPath    = dir </> prog
      , scriptName  = path </> prog
      , pathInfo    = pathinfo
      , queryString = unEscapeString . S.unpack $ uriQuery uri
      }
      where
        (prog,pathinfo) = break (== '/') path0

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
