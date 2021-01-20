module API.ParseConfig.ParseConfigFile where

import Control.Exception (IOException, try)
import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator
import qualified Exceptions.Exceptions as BotEx

getConfigFile :: String -> IO Configurator.Config
getConfigFile path = do
  config <- try $ Configurator.load [Configurator.Required path] :: IO (Either IOException Configurator.Config)
  either BotEx.throwIOException pure config