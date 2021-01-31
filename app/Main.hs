module Main where

import qualified API.Bot as Bot
import qualified Environment.Internals as Env 

main :: IO ()
main = 
  Env.setEnvironment "local.config" >>= Bot.runBot
