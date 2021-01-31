module Environment.Logger.Functions where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Environment.Logger.Structs
  ( LogMessage (..),
    Logger (..),
    Priority,
    startText,
  )

createLogger :: (Monad m) => Priority -> m (Logger IO)
createLogger priority =
  pure $
    Logger
      { botLog = \msg ->
          if getPriority msg >= priority
            then TIO.putStrLn (startText (getPriority msg) <> getText msg)
            else pure ()
      }

makeLogMessage :: LogMessage -> T.Text -> LogMessage
makeLogMessage (LogMessage prior msg) info = LogMessage prior (msg <> info)
