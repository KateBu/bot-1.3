module Environment.Logger.Functions where

import qualified Data.Text as T
import Environment.Logger.Structs (LogMessage (LogMessage))

makeLogMessage :: LogMessage -> T.Text -> LogMessage
makeLogMessage (LogMessage prior msg) info = LogMessage prior (msg <> info)
