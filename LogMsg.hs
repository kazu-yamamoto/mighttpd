module LogMsg (initLog,
               FacilityString, LevelString, LogSystem(..),
               errorMsg, warnMsg, noticeMsg,
               infoMsg, debugMsg) where

import Data.Maybe
import System.Log.Handler.Syslog
import System.Log.Logger

errorMsg :: String -> IO ()
errorMsg = errorM rootLoggerName

warnMsg :: String -> IO ()
warnMsg = warningM rootLoggerName

noticeMsg :: String -> IO ()
noticeMsg = noticeM rootLoggerName

infoMsg :: String -> IO ()
infoMsg = infoM rootLoggerName

debugMsg :: String -> IO ()
debugMsg = debugM rootLoggerName

type FacilityString = String
type LevelString = String

data LogSystem = StdErr | SysLog

initLog :: String
        -> FacilityString
        -> LevelString
        -> LogSystem
        -> IO ()

initLog name fcl lvl SysLog = do
    let level = toLevel lvl
        facility = toFacility fcl
    s <- openlog name [PID] facility level
    updateGlobalLogger rootLoggerName (setLevel level . setHandlers [s])
initLog _ _ lvl StdErr = do
    let level = toLevel lvl
    updateGlobalLogger rootLoggerName (setLevel level)

toLevel :: String -> Priority
toLevel str = fromMaybe (error ("Unknown level " ++ show str))
                        (lookup str levelDB)

toFacility :: String -> Facility
toFacility str = fromMaybe (error ("Unknown facility " ++ show str))
                           (lookup str facilityDB)

levelDB :: [(String, Priority)]
levelDB = [
    ("debug",DEBUG)
  , ("info",INFO)
  , ("notice",NOTICE)
  , ("warning",WARNING)
  , ("error",ERROR)
  , ("critical",CRITICAL)
  , ("alert",ALERT)
  , ("emergency",EMERGENCY)
  ]

facilityDB :: [(String, Facility)]
facilityDB = [
    ("kern",KERN)
  , ("use",USER)
  , ("mail",MAIL)
  , ("daemon",DAEMON)
  , ("auth",AUTH)
  , ("syslog",SYSLOG)
  , ("lpr",LPR)
  , ("news",NEWS)
  , ("uucp",UUCP)
  , ("cron",CRON)
  , ("authpriv",AUTHPRIV)
  , ("ftp",FTP)
  , ("local0",LOCAL0)
  , ("local1",LOCAL1)
  , ("local2",LOCAL2)
  , ("local3",LOCAL3)
  , ("local4",LOCAL4)
  , ("local5",LOCAL5)
  , ("local6",LOCAL6)
  , ("local7",LOCAL7)
  ]
