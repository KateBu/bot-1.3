module Main where

import qualified API.Bot as Bot
import qualified Environment.Environment as Env 

main :: IO ()
main = 
  Env.setEnvironment "config.config" >>= Bot.runBot
