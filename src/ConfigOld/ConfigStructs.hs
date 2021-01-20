module ConfigOld.ConfigStructs where

import Control.Monad ()
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Environment.Logger.Logger as Logger

type Users = Map.Map Int Int

type Token = T.Text

data Config = Config
  { botType :: BotType,
    helpMessage :: T.Text,
    repetition :: Int,
    users :: Users,
    priority :: Logger.Priority
  }
  deriving (Show, Eq)

data BotType = TBot Telegram | VKBot VK
  deriving (Show, Eq)

data Telegram = Telegram
  { tToken :: Token,
    tOffset :: Int
  }
  deriving (Show, Eq)

data VK = VK
  { vkToken :: Token,
    groupID :: Int,
    vkKey :: T.Text,
    vkServer :: T.Text,
    vkTs :: Int
  }
  deriving (Show, Eq)
