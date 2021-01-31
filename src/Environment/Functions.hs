module Environment.Functions where

import qualified Config.Internals as Config
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import qualified Data.Text as T
import qualified Environment.Logger.Internals as Logger
import Environment.Structs (Environment (..))

type REnv m a = ReaderT (Environment m) m a

eConfig :: Monad m => REnv m Config.Config
eConfig = asks config

eRep :: Monad m => REnv m Int
eRep = asks repetition

eHelpMsg :: Monad m => REnv m T.Text
eHelpMsg = asks helpMsg

eLogger :: Monad m => REnv m (Logger.Logger m)
eLogger = asks logger

eGetUid :: Monad m => REnv m Int
eGetUid = asks $ Config.configGetUid . config

updateConfig :: Monad m => Environment m -> Config.Config -> m (Environment m)
updateConfig (Environment _ rep hm lgr) newConfig =
  pure $ Environment newConfig rep hm lgr

eSetOffset :: Monad m => Environment m -> Int -> m (Environment m)
eSetOffset env newOffset = do
  conf <- runReaderT eConfig env
  let newConfig = Config.configSetOffset conf newOffset
  updateConfig env newConfig
