module File (mighty, progName) where

import Control.Applicative
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import IO
import Network.TCPInfo
import Network.URI
import Network.Web.Server
import Network.Web.Server.Basic
import Network.Web.Utils
import System.Directory
import System.FilePath
import System.Posix.Files
import URLMap

progName :: String
progName = "Mighttpd"

progVersion :: String
progVersion = "0.1.0"

progNameVersion :: String
progNameVersion = progName ++ " " ++ progVersion

----------------------------------------------------------------

mighty :: WebConfig -> URLMap -> Handle -> TCPInfo -> IO ()
mighty wcnf umap hdl tcpinfo = do
  let bcnf = BasicConfig { obtain = fileGet
                         , info   = fileInfo
                         , mapper = fileMapper umap
                         , serverName = progNameVersion
                         , tcpInfo = tcpinfo
                         }
  connection hdl (basicServer bcnf) wcnf

----------------------------------------------------------------

lookupFileMap :: URLMap -> URL -> Path
lookupFileMap [] _          = None
lookupFileMap ((from,to):xs) url
    | from `isPrefixOf` url = toPath to $ drop (length from) url
    | otherwise             = lookupFileMap xs url
  where
    toPath (File dir) restPath = File $ dir </> restPath
    toPath (CGI dir _ urlPath) progParam = CGI prog param scriptName
      where
        (prog',param) = break (\x -> x == '?' || x == '/') progParam
        prog = dir </> prog'
        scriptName = urlPath </> prog'
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

fileGet :: FilePath -> Maybe (Integer,Integer) -> IO ByteString
fileGet file Nothing = openFile file ReadMode >>= LBS.hGetContents
fileGet file (Just (skip,len)) = do
    h <- openFile file ReadMode
    hSeek h AbsoluteSeek skip
    LBS.take (fromIntegral len) <$> LBS.hGetContents h

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
