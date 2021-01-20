module Config.Config (module Config) where

import Config.ConfigFunctions as Config
    ( configGetUid, configSetOffset ) 
import Config.ConfigStruct as Config
    ( VK(..), Telegram(..), Config(..), Token ) 
import Config.ConfigData as Config 