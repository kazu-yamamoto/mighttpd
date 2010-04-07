{-# LANGUAGE BangPatterns#-}

module Main where

import Config
import Control.Monad
import File
import IO
import LogMsg
import Network.C10kServer
import Network.Web.Server
import System.Environment
import System.Exit
import System.Posix.Daemonize
import URLMap

----------------------------------------------------------------

main :: IO ()
main = do
    conf <- readFile =<< fileName 0
    mapf <- readFile =<< fileName 1
    let !opt        = parseOption conf
        !webConfig  = toWebConfig opt
        !c10kConfig = toC10kConfig opt
        !uriMap     = parseURLMap mapf
        !prog       = mighty webConfig uriMap
    if opt_debug_mode opt
       then runC10kServerH prog c10kConfig
       else daemonize $ runC10kServerH prog c10kConfig
  where
    fileName n = do
        args <- getArgs
        when (length args /= 2) $ do
            hPutStrLn stderr "Usage: mighttpd config_file uri_map"
            exitFailure
        return $ args !! n

----------------------------------------------------------------

toWebConfig :: Option -> WebConfig
toWebConfig opt = WebConfig {
    closedHook = debugMsg
  , accessHook = noticeMsg
  , errorHook  = warnMsg
  , fatalErrorHook = errorMsg
  , connectionTimer = opt_connection_timer opt
}

toC10kConfig :: Option -> C10kConfig
toC10kConfig opt = C10kConfig {
    initHook = makeInitHook opt
  , exitHook = makeExitHook
  , parentStartedHook = makeParentHook
  , startedHook = makeStartedHook opt
  , sleepTimer = opt_sleep_timer opt
  , preforkProcessNumber = opt_prefork_process_number opt
  , threadNumberPerProcess = opt_thread_number_per_process opt
  , portName = show $ opt_port opt
  , ipAddr = Nothing
  , pidFile = opt_pid_file opt
  , user = opt_user opt
  , group = opt_group opt
}

makeInitHook :: Option -> IO ()
makeInitHook opt =
  if opt_debug_mode opt == True
  then initLog progName "" (opt_log_level opt) StdErr
  else initLog progName (opt_syslog_facility opt) (opt_log_level opt) SysLog

makeExitHook :: String -> IO ()
makeExitHook = errorMsg

makeParentHook :: IO ()
makeParentHook = infoMsg $ progName ++ " started"

makeStartedHook :: Option -> IO ()
makeStartedHook opt =
  if opt_debug_mode opt == True
  then do
    initLog progName "" (opt_log_level opt) StdErr
  else do
    initLog progName (opt_syslog_facility opt) (opt_log_level opt) SysLog
