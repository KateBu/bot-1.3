module Config.Struct where

import qualified Data.Text as T

type Token = T.Text

type Offset = Int

type VKGroup = Int

type VKKey = T.Text

type VKServer = T.Text

data Config = TBot Telegram | VKBot VK
  deriving (Show, Eq)

data Telegram = Telegram
  { tToken :: Token,
    tOffset :: Offset
  }
  deriving (Show, Eq)

data VK = VK
  { vkToken :: Token,
    groupID :: VKGroup,
    vkKey :: VKKey,
    vkServer :: VKServer,
    vkTs :: Offset
  }
  deriving (Show, Eq)
