module Config.Exports (module Config) where

import Config.Data as Config
  ( timeOut,
    vkApiVersion,
    vkLongPollUrl,
  )
import Config.Functions as Config
  ( setOffset,
  )
import Config.Initialization as Config
  ( setBotSettings,
  )
import Config.Struct as Config
  ( BotType,
    Config (..),
    Offset,
    Telegram (..),
    Token,
    VK (..),
    VKGroup,
    VKKey,
    VKServer,
  )
