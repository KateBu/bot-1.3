module Environment.EnvFunctions where

import Control.Monad.Reader
    ( ReaderT, MonadIO(liftIO), MonadReader(ask), asks )
import Environment.EnvStructs ( Env(..) )
import qualified Config.Config as Config 
import qualified Logger.Logger as Logger

type RTEnvIO = ReaderT (Env IO) IO (Env IO)
type RTEnvM m = ReaderT (Env m) m (Env m)

eInitEnv :: Config.Config -> IO (Env IO)
eInitEnv config = 
    Env config <$> (Logger.createLogger $ Config.priority config)

eLog :: Logger.LogMessage ->  ReaderT (Env IO) IO ()
eLog msg = do 
    env <- ask 
    liftIO $ Logger.botLog (eLogger env) msg 

eSetOffset :: (Monad m) => Int -> RTEnvM m
eSetOffset newOffset = do 
    env <- ask 
    let newConfig = Config.configSetOffset (eConfig env) newOffset
    pure $ env {eConfig = newConfig}

eGetConfig :: ReaderT (Env IO) IO Config.Config
eGetConfig = asks eConfig

eSetUserRepeat :: (Monad m) => Int -> Int -> RTEnvM m 
eSetUserRepeat chid newRep = do 
    env <- ask 
    pure $ env {eConfig = Config.setUserRepeat (eConfig env) chid newRep}

eFindUserRepeat :: (Monad m) => Int -> ReaderT (Env m) m Int 
eFindUserRepeat chid = do 
    env <- ask 
    pure $ Config.findUserRepeat (eConfig env) chid 

