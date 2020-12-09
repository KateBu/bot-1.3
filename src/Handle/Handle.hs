module Handle.Handle where

import qualified Config.Config as Config 
import qualified Logger.Logger as Logger 
import qualified Logic.PureStructs as PureStructs 


data Handle m = Handle 
    {
        hConfig :: m (Either Logger.LogMessage Config.Config)
        , hLogger :: m Logger.Logger
        , hGetUpdates :: Config.Config -> m (Either Logger.LogMessage [PureStructs.Message]) 
        , hSendMessage_ :: Config.Config -> PureStructs.Message -> m (Either Logger.LogMessage Config.Config)
    }
