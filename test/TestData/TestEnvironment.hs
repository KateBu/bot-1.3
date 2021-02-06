module TestData.TestEnvironment where

import qualified Config.Exports as Config
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Environment.Logger.Exports as Logger

testEnvVK :: Env.Environment Maybe
testEnvVK =
  Env.Environment testConfigVK 3 helpM testLogger "lkj"

testEnvTelegram :: Env.Environment Maybe
testEnvTelegram =
  Env.Environment testConfigTelegram 5 helpM testLogger "poi"

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
