module Handle.Handle where

import qualified Config.Config as Config 
import qualified Logger.Logger as Logger 
import qualified Logic.PureStructs as PureStructs 
import qualified API.Wrapper as Wrapper 
import qualified Exceptions.Exceptions as BotEx 
{-
data Handle m = Handle 
    {
        hConfig :: m Config.Config
        , hLogger :: m Logger.Logger
        , hGetUpdates :: Config.Config -> Logger.Logger -> m (Either Logger.LogMessage [PureStructs.PureMessage]) 
        , hSendMessage_ :: Config.Config -> Logger.Logger -> PureStructs.PureMessage -> m (Either Logger.LogMessage Config.Config)       
    }
-}
data Handle' m = Handle' 
    {
        hConfig' :: m Config.Config 
        , hLogger' :: m Logger.Logger 
        , hGetUpdates' :: Config.Config -> Logger.Logger 
            -> m (Either BotEx.BotException [PureStructs.PureMessage]) 
        , hSendMessage' :: Config.Config -> Logger.Logger -> PureStructs.PureMessage 
            -> m (Either BotEx.BotException Config.Config)       
    }

new' :: Config.Config -> IO (Handle' IO) 
new' config = pure $ Handle' 
    {
        hConfig' = pure config 
        , hLogger' = Logger.createLogger (Config.priority config)
        , hGetUpdates' = Wrapper.getPureMessageList'   
        , hSendMessage' = Wrapper.sendM' 
    }
{-
new :: Config.Config -> IO (Handle IO) 
new config = pure $ Handle 
    {
        hConfig = pure config 
        , hLogger = Logger.createLogger (Config.priority config)
        , hGetUpdates = Wrapper.getPureMessageList  
        , hSendMessage_ = Wrapper.sendM 
    }
-}