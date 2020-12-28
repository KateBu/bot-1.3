module TestData.TestConfig where

import qualified Data.Map as Map 
import qualified Config.Config as Config 
import qualified Logger.Logger as Logger 

users :: Config.Users 
users = Map.fromList [(11,1), (22,2), (33,3)]

testConfigVK :: Config.Config 
testConfigVK = Config.Config  
    (Config.VKBot (Config.VK "token" 11 "key" "server" 0))
    "VK help message"
    1 
    users 
    Logger.Debug 

testConfigTelegram :: Config.Config 
testConfigTelegram = Config.Config 
    (Config.TBot (Config.Telegram "token" 0))
    "Telegram help message"
    1 
    users 
    Logger.Error 