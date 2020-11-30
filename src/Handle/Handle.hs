module Handle.Handle where

import qualified Data.Text as T

import Config.Config 
import Logger.Logger 
import API.Telegram.Structs
import Logic.PureStructs

type Err = T.Text

data Handle m = Handle 
    {
        hConfig :: m (Either LogMessage Config)
        , hLogger :: m Logger
        , hGetUpdates :: m (Either LogMessage [Message]) 
        , hSendMessage_ :: Config -> Message -> m (Either LogMessage Config)
    }


type User = (Integer, Int)
