module Exceptions.ExStructs where

import Control.Exception (Exception, IOException)
import qualified Logger.Logger as Logger

data BotException
  = InitConfigExcept Logger.LogMessage
  | IOExcept IOException
  | ParseExcept Logger.LogMessage
  | UpdateExcept Logger.LogMessage
  | SendExcept Logger.LogMessage
  | HttpExcept Logger.LogMessage
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
  show (OtherExcept oEx) =
    "Other exception occured -- " <> show oEx

instance Exception BotException
