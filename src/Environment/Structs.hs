module Environment.Structs where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Environment.Config.Exports as Config
import qualified Environment.Logger.Exports as Logger

type HelpMessage = T.Text

type RepeatNumber = Int

type BotType = T.Text

type DBConnectString = BS.ByteString

data Environment m = Environment
  { config :: Config.Config,
    repetition :: RepeatNumber,
    helpMsg :: HelpMessage,
    logger :: Logger.Logger m,
    dbConnectString :: DBConnectString
  }
