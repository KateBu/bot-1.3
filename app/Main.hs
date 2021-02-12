module Main where

import qualified Bot
import qualified Environment.Exports as Env 

main :: IO ()
main = 
  Env.setEnvironment "config.config" >>= Bot.runBot
