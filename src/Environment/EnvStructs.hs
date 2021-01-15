module Environment.EnvStructs where

import Config.Config (Config)
import Logger.Logger (Logger)

data Env m = Env
  { eConfig :: Config,
    eLogger :: Logger m
  }
