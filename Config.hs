{-# LANGUAGE TypeSynonymInstances #-}

module Config (Option(..), parseOption, defaultOption) where

import Text.ParserCombinators.Parsec
import Data.List (isPrefixOf)

----------------------------------------------------------------

defaultOption :: Option
defaultOption = Option {
    opt_port = 8080
  , opt_prefork_process_number = 20
  , opt_thread_number_per_process = 500
  , opt_connection_timer = 10
  , opt_sleep_timer = 2
  , opt_debug_mode = True
  , opt_user = "nobody"
  , opt_group = "nobody"
  , opt_syslog_facility = "local5"
  , opt_log_level = "info"
  , opt_pid_file = "/var/run/mighttpd.pid"
}

data Option = Option {
    opt_port :: Int
  , opt_prefork_process_number :: Int
  , opt_thread_number_per_process :: Int
  , opt_connection_timer :: Int
  , opt_sleep_timer :: Int
  , opt_debug_mode :: Bool
  , opt_user :: String
  , opt_group :: String
  , opt_syslog_facility :: String
  , opt_log_level :: String
  , opt_pid_file :: String
} deriving Show

----------------------------------------------------------------

parseOption :: String -> Option
parseOption = makeOpt defaultOption . parseConfig

----------------------------------------------------------------

makeOpt :: Option -> [Conf] -> Option
makeOpt def conf = Option {
    opt_port = get "Port" opt_port
  , opt_prefork_process_number = get "Prefork_Process_Number" opt_prefork_process_number
  , opt_thread_number_per_process = get "Thread_Number_Per_Process" opt_thread_number_per_process
  , opt_connection_timer = get "Connection_Timer" opt_connection_timer
  , opt_sleep_timer      = get "Sleep_Timer" opt_sleep_timer
  , opt_debug_mode       = get "Debug_Mode" opt_debug_mode
  , opt_user             = get "User" opt_user
  , opt_group            = get "Group" opt_group
  , opt_syslog_facility  = get "Syslog_Facility" opt_syslog_facility
  , opt_log_level        = get "Log_Level" opt_log_level
  , opt_pid_file         = get "Pid_File" opt_pid_file
  }
    where
      get key func = case lookup key conf of
                       Nothing -> func def
                       Just x  -> fromConf x

----------------------------------------------------------------

type Conf = (String, ConfValue)

data ConfValue = CV_Int Int | CV_Bool Bool | CV_String String deriving Show

class FromConf a where
    fromConf :: ConfValue -> a

instance FromConf Int where
    fromConf (CV_Int n) = n
    fromConf _ = error "fromConf int"

instance FromConf Bool where
    fromConf (CV_Bool b) = b
    fromConf _ = error "fromConf bool"

instance FromConf String where
    fromConf (CV_String s) = s
    fromConf _ = error "fromConf string"

----------------------------------------------------------------

parseConfig :: String -> [Conf]
parseConfig cs = let css = filter (not.isPrefixOf "#") $ lines cs
                 in map parseConf css
    where
      parseConf xs = case parse config "config" xs of
                       Right cnf -> cnf
                       Left  err -> error $ "parseConfig " ++ show err

----------------------------------------------------------------

config :: Parser Conf
config = do nm <- name
            spaces
            char ':'
            spaces
            vl <- value
            return (nm,vl)

name :: Parser String
name = many1.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

value :: Parser ConfValue
value = choice [try cv_int, try cv_bool, cv_string]

cv_int :: Parser ConfValue
cv_int = do ns <- many1 digit
            spaces
            eof
            return $ CV_Int $ read ns

cv_bool :: Parser ConfValue
cv_bool = do { string "Yes"; spaces; eof; return (CV_Bool True)  } <|>
          do { string "No";  spaces; eof; return (CV_Bool False) }

cv_string :: Parser ConfValue
cv_string = do ss <- many1 (noneOf " \t\n")
               return $ CV_String ss
