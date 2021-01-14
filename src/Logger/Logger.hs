module Logger.Logger where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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

createLogger :: (Monad m) => Priority -> m (Logger IO)
createLogger priority =
  pure $
    Logger
      { botLog = \msg ->
          if getPriority msg >= priority
            then TIO.putStrLn (startText (getPriority msg) <> getText msg)
            else pure ()
      }

startText :: Priority -> T.Text
startText Debug = "Debug: "
startText Info = "Info: "
startText Warning = "Warning: "
startText Error = "ERROR: "

makeLogMessage :: LogMessage -> T.Text -> LogMessage
makeLogMessage (LogMessage prior msg) info = LogMessage prior (msg <> info)
