module Config.Config (module Config) where

import Config.ConfigStructs as Config
    ( Token, Users, VK(..), Telegram(..), BotType(..), Config(..) )
import Config.ConfigData as Config
    ( timeOut, vkApiVersion, vkLongPollUrl )
import Config.MakeConfigFunctions as Config
    ( parseConfig )   
import Config.ConfigFunctions as Config
    ( addUser,
      configGetUid,
      configSetOffset,
      deleteUser,
      findUserRepeat,
      reWriteUserRepeat,
      setUserRepeat )
    
    









