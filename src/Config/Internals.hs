module Config.Internals (module Config) where

import Config.Data as Config
  ( timeOut,
    vkApiVersion,
    vkLongPollUrl,
  )
import Config.Functions as Config
  ( configGetUid,
    configSetOffset,
  )
import Config.Struct as Config
  ( Config (..),
    Offset,
    Telegram (..),
    Token,
    VK (..),
    VKGroup,
    VKKey,
    VKServer,
  )
