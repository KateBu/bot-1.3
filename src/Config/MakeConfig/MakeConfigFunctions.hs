module Config.MakeConfig.MakeConfigFunctions where

import qualified Config.ConfigStructs as Config
import Config.MakeConfig.InitConfig (initConfig)
import Config.MakeConfig.SetBotSetting (setBotTypeSettings)
import Control.Exception (IOException, try)
import Control.Monad ()
import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator
import qualified Data.Text as T
import qualified Exceptions.Exceptions as BotEx

parseConfig :: String -> IO Config.Config
parseConfig path = do
  getConfigFile path >>= parseConfigFile

getConfigFile :: String -> IO Configurator.Config
getConfigFile path = do
  config <- try $ Configurator.load [Configurator.Required path] :: IO (Either IOException Configurator.Config)
  either BotEx.throwIOException pure config

parseConfigFile ::
  Configurator.Config ->
  IO Config.Config
parseConfigFile conf = do
  botT <- Configurator.lookup conf (T.pack "bot.botType") :: IO (Maybe T.Text)
  rep <- Configurator.lookup conf (T.pack "bot.repetition") :: IO (Maybe Int)
  msg <- Configurator.lookup conf (T.pack "bot.helpMessage") :: IO (Maybe T.Text)
  prior <- Configurator.lookup conf (T.pack "bot.logPriority") :: IO (Maybe String)
  tTok <- Configurator.lookup conf (T.pack "bot.telegramToken") :: IO (Maybe Config.Token)
  vkTok <- Configurator.lookup conf (T.pack "bot.VKToken") :: IO (Maybe Config.Token)
  vkGroup <- Configurator.lookup conf (T.pack "bot.VKGroupID") :: IO (Maybe Int)
  setBotTypeSettings botT vkGroup vkTok tTok >>= initConfig msg rep prior
