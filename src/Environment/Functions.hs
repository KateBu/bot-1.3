module Environment.Functions where

import Control.Monad.Reader (ReaderT (runReaderT), asks)
import qualified Environment.Config.Exports as Config
import qualified Environment.Logger.Exports as Logger
import qualified Environment.Structs as Env

type REnv m a = ReaderT (Env.Environment m) m a

eConfig :: Monad m => REnv m Config.Config
eConfig = asks Env.config

eRep :: Monad m => REnv m Env.RepeatNumber
eRep = asks Env.repetition

eHelpMsg :: Monad m => REnv m Env.HelpMessage
eHelpMsg = asks Env.helpMsg

eLogger :: Monad m => REnv m (Logger.Logger m)
eLogger = asks Env.logger

eDBConnectionString :: Monad m => REnv m Env.DBConnectString
eDBConnectionString = asks Env.dbConnectString

updateConfig :: Monad m => Env.Environment m -> Config.Config -> m (Env.Environment m)
updateConfig (Env.Environment _ repeatNimber helpMsg logger dbConnectString) newConfig =
  pure $ Env.Environment newConfig repeatNimber helpMsg logger dbConnectString

eSetOffset :: Monad m => Env.Environment m -> Config.Offset -> m (Env.Environment m)
eSetOffset env newOffset = do
  conf <- runReaderT eConfig env
  let newConfig = Config.configSetOffset conf newOffset
  updateConfig env newConfig
