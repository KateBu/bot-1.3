module Config.ConfigStruct where

import qualified Data.Text as T 

type Token = T.Text

data Config = TBot Telegram | VKBot VK
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