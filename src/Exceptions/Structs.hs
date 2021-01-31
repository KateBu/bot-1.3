module Exceptions.Structs where

import Control.Exception (Exception, IOException)
import Database.PostgreSQL.Simple
  ( FormatError,
    QueryError,
    ResultError,
    SqlError,
  )
import qualified Environment.Logger.Internals as Logger

data BotException
  = InitConfigExcept Logger.LogMessage
  | IOExcept IOException
  | ParseExcept Logger.LogMessage
  | UpdateExcept Logger.LogMessage
  | SendExcept Logger.LogMessage
  | HttpExcept Logger.LogMessage
  | DBFormatError FormatError
  | DBQueryError QueryError
  | DBResultError ResultError
  | DBSqlError SqlError
  | OtherExcept Logger.LogMessage
  deriving (Eq)

instance Show BotException where
  show (InitConfigExcept logMsg) =
    "InitConfigException occured -- " <> show logMsg
  show (IOExcept ioEx) =
    "IO exception occured -- " <> show ioEx
  show (ParseExcept logMsg) =
    "Parsing exception occured -- " <> show logMsg
  show (UpdateExcept logMsg) =
    "Update exception occured -- " <> show logMsg
  show (SendExcept logMsg) =
    "Send exception occured -- " <> show logMsg
  show (HttpExcept logMsg) =
    "Http exception occured -- " <> show logMsg
  show (DBFormatError fErr) =
    "Database format error occured -- " <> show fErr
  show (DBQueryError qErr) =
    "Database query error occured -- " <> show qErr
  show (DBResultError rErr) =
    "Database result error occured -- " <> show rErr
  show (DBSqlError sqlErr) =
    "Database sql error occured -- " <> show sqlErr
  show (OtherExcept oEx) =
    "Other exception occured -- " <> show oEx

instance Exception BotException
