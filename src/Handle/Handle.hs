module Handle.Handle where

import Control.Exception ( bracket )

import qualified Config.Config as Config 
import qualified Logger.Logger as Logger 
import qualified Logic.PureStructs as PureStructs 
import qualified API.Wrapper as Wrapper 

data Handle m = Handle 
    {
        hConfig :: m Config.Config
        , hLogger :: m Logger.Logger
        , hGetUpdates :: Config.Config -> Logger.Logger -> m (Either Logger.LogMessage [PureStructs.PureMessage]) 
        , hSendMessage_ :: Config.Config -> Logger.Logger -> PureStructs.PureMessage -> m (Either Logger.LogMessage Config.Config)       
    }

new :: Config.Config -> IO (Handle IO) 
new config = pure $ Handle 
    {
        hConfig = pure config 
        , hLogger = Logger.createLogger (Config.priority config)
        , hGetUpdates = Wrapper.getPureMessageList  
        , hSendMessage_ = Wrapper.sendM 
    }

close :: Handle m -> IO ()
close _ = pure ()

withHandleNoParams :: Config.Config -> (Handle IO -> IO a) -> IO a 
withHandleNoParams config = 
    bracket (new config) close 

withHandle :: Config.Config -> (Handle IO -> c -> IO a) -> c -> IO a 
withHandle config func params = 
    bracket (new config) close (flip func params)

