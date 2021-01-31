module Environment.Structs where

import qualified Config.Internals as Config
import qualified Data.Text as T
import qualified Environment.Logger.Internals as Logger

data Environment m = Environment
  { config :: Config.Config,
    repetition :: Int,
    helpMsg :: T.Text,
    logger :: Logger.Logger m
  }
