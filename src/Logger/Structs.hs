module Logger.Structs where

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
  show (LogMessage priority txt) = T.unpack $ startText priority <> txt

newtype Logger m = Logger
  { botLog :: LogMessage -> m ()
  }

startText :: Priority -> T.Text
startText Debug = "Debug: "
startText Info = "Info: "
startText Warning = "Warning: "
startText Error = "ERROR: "
