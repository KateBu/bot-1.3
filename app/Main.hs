module Main where

import qualified Bot
import qualified Environment.Exports as Env 

main :: IO ()
main = 
  Env.setEnvironment "local.config" >>= Bot.runBot
