module Environment.EnvFunctions where

import Control.Monad.Reader ( ReaderT, asks ) 
import qualified Data.Text as T 
import qualified Config.Config as Config 
import qualified Environment.Logger.Logger as Logger
import Environment.EnvStructs ( Environment(..) ) 

type RMA m a = ReaderT (Environment m) m a 

eConfig :: Monad m => RMA m Config.Config 
eConfig = asks config 

eRep :: Monad m => RMA m Int 
eRep = asks repetition

eHelpMsg :: Monad m => RMA m T.Text  
eHelpMsg = asks helpMsg

eGetLogger :: Monad m => RMA m (Logger.Logger m)
eGetLogger = asks logger 

eGetUid :: Monad m => RMA m Int 
eGetUid = asks $ Config.configGetUid . config     

updateConfig :: Environment m -> Config.Config -> Environment m
updateConfig (Environment _ rep hm logger) newConfig= 
    Environment newConfig rep hm logger

