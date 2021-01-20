module Environment.EnvStructs where

import qualified Data.Text as T 
import qualified Config.Config  as Config 
import qualified Environment.Logger.Logger  as Logger 

data Environment m = Environment 
  {
    config :: Config.Config, 
    repetition :: Int,
    helpMsg :: T.Text ,
    logger :: Logger.Logger m
  } 

