module Environment.Config.Exports (module Config) where

import Environment.Config.Data as Config
  ( timeOut,
    vkApiVersion,
    vkLongPollUrl,
  )
import Environment.Config.Functions as Config
  ( configSetOffset,
  )
import Environment.Config.Struct as Config
  ( Config (..),
    Offset,
    Telegram (..),
    Token,
    VK (..),
    VKGroup,
    VKKey,
    VKServer,
  )
