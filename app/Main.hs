module Main where

import qualified Data.Text.IO as TIO 
import qualified API.Bot as Bot 
import qualified Config.Config as Config 
import qualified Logger.LoggerMsgs as LoggerMsgs 

configPath :: String 
configPath = "config.config"

main :: IO ()
main = do
    config <- Config.parseConfig configPath 
    case config of 
        Just existedConfig -> Bot.runBot existedConfig
        _ -> TIO.putStrLn LoggerMsgs.fatalConfig 
