module Environment.EnvFunctions where

import qualified Config.Config as Config
import Control.Monad.Reader
import qualified Data.Text as T
import Environment.EnvStructs (Environment (..))
import qualified Environment.Logger.Logger as Logger

type RMA m a = ReaderT (Environment m) m a

eConfig :: Monad m => RMA m Config.Config
eConfig = asks config

eRep :: Monad m => RMA m Int
eRep = asks repetition

eHelpMsg :: Monad m => RMA m T.Text
eHelpMsg = asks helpMsg

eLogger :: Monad m => RMA m (Logger.Logger m)
eLogger = asks logger

eGetUid :: Monad m => RMA m Int
eGetUid = asks $ Config.configGetUid . config

updateConfig :: Monad m => Environment m -> Config.Config -> m (Environment m)
updateConfig (Environment _ rep hm logger) newConfig =
  pure $ Environment newConfig rep hm logger

eSetOffset :: Monad m => Environment m -> Int -> m (Environment m)
eSetOffset env newOffset = do
  config <- runReaderT eConfig env
  let newConfig = Config.configSetOffset config newOffset
  updateConfig env newConfig
