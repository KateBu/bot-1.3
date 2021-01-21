module Environment.InitEnvironment.InitEnvironment where

import qualified Config.Config as Config
import qualified Data.Text as T
import qualified Environment.EnvStructs as Env
import qualified Environment.Logger.Logger as Logger
import qualified Environment.Logger.LoggerMsgs as LoggerMsgs
import qualified Exceptions.Exceptions as BotEx
import Text.Read (readMaybe)

initEnvironment ::
  Maybe T.Text ->
  Maybe Int ->
  Maybe String ->
  Config.Config ->
  IO (Env.Environment IO)
initEnvironment (Just helpMsg) (Just rep) (Just priorStr) config = do
  let prior = readMaybe priorStr :: Maybe Logger.Priority
  maybe (BotEx.throwOtherException LoggerMsgs.initLogFld) (makeEnv helpMsg rep config) prior
initEnvironment _ _ _ _ = BotEx.throwInitConfigExcept

makeEnv :: T.Text -> Int -> Config.Config -> Logger.Priority -> IO (Env.Environment IO)
makeEnv hm rep config prior = do
  logger <- Logger.createLogger prior
  pure $ Env.Environment config rep hm logger

checkRepNumber :: Int -> Int
checkRepNumber val
  | val <= 1 = 1
  | val >= 5 = 5
  | otherwise = val
