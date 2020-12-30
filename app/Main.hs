module Main where

import qualified API.Bot as Bot 
import qualified Config.Config as Config 
import qualified Exceptions.Exceptions as BotEx 

configPath :: String 
configPath = "config.config"

main :: IO ()
main = do
    config <- Config.parseConfig configPath 
    either BotEx.handleBotException Bot.runBot config 
    
 