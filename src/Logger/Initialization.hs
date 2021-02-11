module Logger.Initialization where

import qualified Data.Text.IO as TIO
import Logger.Structs
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
