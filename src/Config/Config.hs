module Config.Config (module Config) where

import Config.ConfigData as Config
  ( timeOut,
    vkApiVersion,
    vkLongPollUrl,
  )
import Config.ConfigFunctions as Config
  ( configGetUid,
    configSetOffset,
  )
import Config.ConfigStruct as Config
  ( Config (..),
    Telegram (..),
    Token,
    VK (..),
  )
