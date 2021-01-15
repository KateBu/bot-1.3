module Environment.EnvFunctions where

import qualified Config.Config as Config
import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
    evalStateT,
    execStateT,
    gets,
  )
import Environment.EnvStructs (Env (..))
import qualified Logger.Logger as Logger

type STEmvM m = StateT (Env m) m (Env m)

eInitEnv :: Config.Config -> IO (Env IO)
eInitEnv config =
  Env config <$> Logger.createLogger (Config.priority config)

stLog :: Monad m => Logger.LogMessage -> StateT (Env m) m ()
stLog msg = do
  state <- get
  lift $ Logger.botLog (eLogger state) msg

eLog :: Monad m => Logger.LogMessage -> Env m -> m ()
eLog msg = evalStateT (stLog msg)

stSetOffset :: (Monad m) => Int -> STEmvM m
stSetOffset newOffset = do
  state <- get
  let newConfig = Config.configSetOffset (eConfig state) newOffset
  put $ state {eConfig = newConfig}
  pure state

eSetOffset :: (Monad m) => Int -> Env m -> m (Env m)
eSetOffset newOffset = execStateT $ stSetOffset newOffset

stFindUserRepeat :: (Monad m) => Int -> StateT (Env m) m Int
stFindUserRepeat chid = do
  state <- get
  pure $ Config.findUserRepeat (eConfig state) chid

eFindUserRepeat :: (Monad m) => Int -> Env m -> m Int
eFindUserRepeat chid = evalStateT $ stFindUserRepeat chid

stSetUserRepeat :: (Monad m) => Int -> Int -> STEmvM m
stSetUserRepeat chid newRep = do
  state <- get
  put $ state {eConfig = Config.setUserRepeat (eConfig state) chid newRep}
  pure state

eSetUserRepeat :: Monad m => Int -> Int -> Env m -> m (Env m)
eSetUserRepeat chid newRep = execStateT (stSetUserRepeat chid newRep)

stGetConfig :: Monad m => StateT (Env m) m Config.Config
stGetConfig = gets eConfig

eGetConfig :: Monad m => Env m -> m Config.Config
eGetConfig = evalStateT stGetConfig

stGetBotType :: Monad m => StateT (Env m) m Config.BotType
stGetBotType =
  gets $ Config.botType . eConfig

eGetBotType :: Monad m => Env m -> m Config.BotType
eGetBotType = evalStateT stGetBotType
