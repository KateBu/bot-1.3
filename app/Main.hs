module Main where

import qualified API.Bot as Bot
import qualified Config.Config as Config

main :: IO ()
main =
  Config.parseConfig' "config.config" >>= Bot.runBot'
