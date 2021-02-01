module Environment.SetEnvironment where

import qualified Config.Internals as Config
import Control.Exception (IOException, try)
import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator
import Environment.BotSettings.SetBotSettings
  ( setBotTypeSettings,
  )
import qualified Environment.Logger.Internals as Logger
import qualified Environment.Logger.Messages as LoggerMsgs
import qualified Environment.Structs as Env
import qualified Exceptions.Internals as BotEx
import Text.Read (readMaybe)

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
  setBotTypeSettings botT vkGroup vkTok tTok >>= initEnvironment msg rep prior

initEnvironment ::
  Maybe Env.HelpMessage ->
  Maybe Env.RepeatNumber ->
  Maybe String ->
  Config.Config ->
  IO (Env.Environment IO)
initEnvironment (Just helpMsg) (Just rep) (Just priorStr) config = do
  let mbPrior = readMaybe priorStr :: Maybe Logger.Priority
  maybe (BotEx.throwOtherException LoggerMsgs.initLogFld) (makeEnv helpMsg rep config) mbPrior
initEnvironment _ _ _ _ = BotEx.throwInitConfigExcept

makeEnv ::
  Env.HelpMessage ->
  Env.RepeatNumber ->
  Config.Config ->
  Logger.Priority ->
  IO (Env.Environment IO)
makeEnv hm rep config prior = do
  logger <- Logger.createLogger prior
  pure $ Env.Environment config (checkRepNumber rep) hm logger

checkRepNumber :: Env.RepeatNumber -> Env.RepeatNumber
checkRepNumber val
  | val <= 1 = 1
  | val >= 5 = 5
  | otherwise = val
