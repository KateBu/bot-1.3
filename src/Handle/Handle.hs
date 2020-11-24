module Handle.Handle where

import qualified Data.Text as T

import Config.Config 
import Logger.Logger 
import API.Telegram.Structs
import Logic.PureStructs

type Err = T.Text

data Handle m = Handle 
    {
        hConfig :: m Config
        , hLogger :: m Logger
        , hGetUpdates :: m (Either Err [Message]) 
        , hSendMessage :: Message -> m (Maybe Config) 
        , hSendMessage_ :: Message -> m (Maybe ())
        , hSetOffset :: Integer -> m Config
        , hEditUsers :: Config -> User -> m (Maybe Config)
    }


type User = (Integer, Int)
