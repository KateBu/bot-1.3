module Exceptions.Exceptions where

import qualified Data.Text as T
import Control.Exception ( Exception, IOException ) 
import qualified Logger.Logger as Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs

data BotException = InitConfigExcept Logger.LogMessage 
    | IOExept IOException
    | ParseExcept Logger.LogMessage

instance Show BotException where 
    show (InitConfigExcept logMsg) = 
        "InitConfigException occured -- " <> show logMsg
    show (IOExept logMsg) =
        "IO exception occured -- " <> show logMsg
    show (ParseExcept logMsg) = 
        "Parsing exceptions occured" <> show logMsg 

instance Exception BotException 

handleBotException :: BotException -> IO ()
handleBotException ex = do 
    print ex 
    putStrLn "Program terminated"

throwBotExcept :: Monad m => BotException -> m (Either BotException a)
throwBotExcept err = pure $ Left err 

throwInitConfigExcept :: Monad m => m (Either BotException a)
throwInitConfigExcept = throwBotExcept $ InitConfigExcept LoggerMsgs.initConfigExcept

throwParseExcept :: Monad m => String -> m (Either BotException a)
throwParseExcept err = throwBotExcept $ ParseExcept (Logger.makeLogMessage LoggerMsgs.parseErr (T.pack err))

catchBotExcept :: Monad m => (BotException -> Either BotException a)
    -> Either BotException a 
    -> m (Either BotException a) 
catchBotExcept _ (Right smth) = pure $ Right smth 
catchBotExcept handler (Left exception) = 
    pure $ handler exception 





