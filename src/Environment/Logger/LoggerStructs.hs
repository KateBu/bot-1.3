module Environment.Logger.LoggerStructs where

import qualified Data.Text as T

data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show, Read)

data LogMessage = LogMessage
  { getPriority :: Priority,
    getText :: T.Text
  }
  deriving (Eq)

instance Show LogMessage where
  show (LogMessage prior txt) = T.unpack $ startText prior <> txt

data Logger m = Logger
  { botLog :: LogMessage -> m ()
  }

startText :: Priority -> T.Text
startText Debug = "Debug: "
startText Info = "Info: "
startText Warning = "Warning: "
startText Error = "ERROR: "