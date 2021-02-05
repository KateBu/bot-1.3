module Environment.SetEnvironment where

import qualified Config.Exports as Config
import Control.Exception (IOException, try)
import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator
import Environment.BotSettings.SetBotSettings
  ( setBotSettings,
  )
import qualified Environment.Logger.Exports as Logger
import qualified Environment.Structs as Env
import qualified Exceptions.Exports as BotEx
import Text.Read (readMaybe)
import qualified TextMessages.LoggerMessages as LoggerMsgs

setEnvironment :: String -> IO (Env.Environment IO)
setEnvironment path = do
  getConfigFile path >>= setEnvironment'

getConfigFile :: String -> IO Configurator.Config
getConfigFile path = do
  eiConfig <- try $ Configurator.load [Configurator.Required path] :: IO (Either IOException Configurator.Config)
  either BotEx.throwIOException pure eiConfig

setEnvironment' ::
  Configurator.Config ->
  IO (Env.Environment IO)
setEnvironment' conf = do
  botT <- Configurator.lookup conf "bot.botType" :: IO (Maybe Env.BotType)
  rep <- Configurator.lookup conf "bot.repetition" :: IO (Maybe Env.RepeatNumber)
  msg <- Configurator.lookup conf "bot.helpMessage" :: IO (Maybe Env.HelpMessage)
  tTok <- Configurator.lookup conf "bot.telegramToken" :: IO (Maybe Config.Token)
  prior <- Configurator.lookup conf "bot.logPriority" :: IO (Maybe String)
  vkTok <- Configurator.lookup conf "bot.VKToken" :: IO (Maybe Config.Token)
  vkGroup <- Configurator.lookup conf "bot.VKGroupID" :: IO (Maybe Config.VKGroup)
  dbCnt <- Configurator.lookup conf "bot.dbConnectString" :: IO (Maybe Env.DBConnectString)
  botSettings <- setBotSettings botT vkGroup vkTok tTok
  initEnvironment msg rep prior dbCnt botSettings

initEnvironment ::
  Maybe Env.HelpMessage ->
  Maybe Env.RepeatNumber ->
  Maybe String ->
  Maybe Env.DBConnectString ->
  Config.Config ->
  IO (Env.Environment IO)
initEnvironment (Just helpMsg) (Just repeatNumber) (Just priorityStr) (Just dbConnectString) config = do
  let mbPriority = readMaybe priorityStr :: Maybe Logger.Priority
  maybe
    (BotEx.throwOtherException LoggerMsgs.initLogFailed)
    (initEnvironment' helpMsg repeatNumber dbConnectString config)
    mbPriority
initEnvironment _ _ _ _ _ = BotEx.throwInitConfigExcept

initEnvironment' ::
  Env.HelpMessage ->
  Env.RepeatNumber ->
  Env.DBConnectString ->
  Config.Config ->
  Logger.Priority ->
  IO (Env.Environment IO)
initEnvironment' helpMsg repeatNumber dbConnectString config priority = do
  logger <- Logger.createLogger priority
  pure $ Env.Environment config (checkRepeatNumber repeatNumber) helpMsg logger dbConnectString

checkRepeatNumber :: Env.RepeatNumber -> Env.RepeatNumber
checkRepeatNumber val
  | val <= 1 = 1
  | val >= 5 = 5
  | otherwise = val
