module TestData.TestEnvironment where

import qualified Config.Internals as Config
import qualified Data.Text as T
import qualified Environment.Internals as Env
import qualified Environment.Logger.Internals as Logger

testEnvVK :: Env.Environment Maybe
testEnvVK =
  Env.Environment testConfigVK 3 helpM testLogger

testEnvTelegram :: Env.Environment Maybe
testEnvTelegram =
  Env.Environment testConfigTelegram 5 helpM testLogger

helpM :: T.Text
helpM = "help command response"

testConfigVK :: Config.Config
testConfigVK =
  Config.VKBot $ Config.VK "token" 11 "key" "server" 5

testConfigTelegram :: Config.Config
testConfigTelegram =
  Config.TBot $ Config.Telegram "token" 10

testLogger :: Logger.Logger Maybe
testLogger = Logger.Logger {Logger.botLog = const $ Just ()}
