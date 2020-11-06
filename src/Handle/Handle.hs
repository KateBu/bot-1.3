module Handle.Handle where

import qualified Data.Text as T

import Config.Config 
import Logger.Logger 
import API.Telegram.Structs
import Logic.PureStructs

data Handle m = Handle 
    {
        hConfig :: m Config 
        , hLogger :: m Logger
        , hGetUpdates :: Logger -> m (Maybe Updates) 
        , hSendMessage :: Message -> m Config 
        , hSendMessage_ :: Message -> m ()
        , hSetOffset :: Integer -> m Config 
        , hEditUsers :: Config -> User -> m Config
    }


type User = (Integer, Int)
