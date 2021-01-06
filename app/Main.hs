module Main where

import qualified API.Bot as Bot
import qualified Config.Config as Config

configPath :: String
configPath = "config.config"

main :: IO ()
main =
  Config.parseConfig configPath >>= Bot.runBot
