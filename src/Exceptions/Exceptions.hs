module Exceptions.Exceptions where

import Control.Exception 
import qualified Logger.Logger as Logger 

data BotException = InitConfigException Logger.LogMessage | IOExept IOException

instance Show BotException where 
    show (InitConfigException logMsg) = 
        "InitConfigException occured -- " <> show logMsg
    show (IOExept logMsg) =
        "IO exception occured -- " <> show logMsg

instance Exception BotException 

handleBotException :: BotException -> IO ()
handleBotException ex = do 
    print ex 
    putStrLn "Program terminated"


