module Environment.Config.Exports (module Config) where

import Environment.Config.Data as Config
  ( timeOut,
    vkApiVersion,
    vkLongPollUrl,
  )
import Environment.Config.Functions as Config
  ( setOffset,
  )
import Environment.Config.Initialization as Config
  ( setBotSettings,
  )
import Environment.Config.Struct as Config
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
