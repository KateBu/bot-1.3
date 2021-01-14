module Main where

import qualified API.Bot as Bot
import qualified Config.Config as Config
import qualified Environment.Environment as Env 

main :: IO ()
main =
  Config.parseConfig "config.config" >>= Env.eInitEnv >>= Bot.runBot
