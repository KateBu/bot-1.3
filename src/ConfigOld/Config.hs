module ConfigOld.Config (module Config) where

import ConfigOld.ConfigData as Config
  ( timeOut,
    vkApiVersion,
    vkLongPollUrl,
  )
import ConfigOld.ConfigFunctions as Config
  ( addUser,
    configGetUid,
    configSetOffset,
    deleteUser,
    findUserRepeat,
    reWriteUserRepeat,
    setUserRepeat,
  )
import ConfigOld.ConfigStructs as Config
  ( BotType (..),
    Config (..),
    Telegram (..),
    Token,
    Users,
    VK (..),
  )
import ConfigOld.MakeConfig.MakeConfigFunctions as Config
  ( parseConfig,
  )
