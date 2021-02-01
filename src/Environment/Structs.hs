module Environment.Structs where

import qualified Config.Internals as Config
import qualified Data.Text as T
import qualified Environment.Logger.Internals as Logger

type HelpMessage = T.Text

type RepeatNumber = Int

type BotType = T.Text

data Environment m = Environment
  { config :: Config.Config,
    repetition :: RepeatNumber,
    helpMsg :: HelpMessage,
    logger :: Logger.Logger m
  }
