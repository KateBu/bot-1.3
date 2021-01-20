module Environment.InitEnvironment where

import qualified Data.Text as T
import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator
import API.ParseConfig.ParseConfigFile ( getConfigFile ) 
import Environment.InitEnvironment.SetBotSettings
    ( setBotTypeSettings )
import Environment.InitEnvironment.InitEnvironment
    ( initEnvironment )
import qualified Config.Config as Config 
import qualified Environment.EnvStructs as Env 


setEnvironment :: String -> IO (Env.Environment IO)
setEnvironment path = do
  getConfigFile path >>= setEnvironment' 

setEnvironment' ::
  Configurator.Config ->
  IO (Env.Environment IO)
setEnvironment' conf = do
  botT <- Configurator.lookup conf (T.pack "bot.botType") :: IO (Maybe T.Text)
  rep <- Configurator.lookup conf (T.pack "bot.repetition") :: IO (Maybe Int)
  msg <- Configurator.lookup conf (T.pack "bot.helpMessage") :: IO (Maybe T.Text)
  tTok <- Configurator.lookup conf (T.pack "bot.telegramToken") :: IO (Maybe Config.Token)
  prior <- Configurator.lookup conf (T.pack "bot.logPriority") :: IO (Maybe String)
  vkTok <- Configurator.lookup conf (T.pack "bot.VKToken") :: IO (Maybe Config.Token)
  vkGroup <- Configurator.lookup conf (T.pack "bot.VKGroupID") :: IO (Maybe Int)
  setBotTypeSettings botT vkGroup vkTok tTok >>= initEnvironment msg rep prior
