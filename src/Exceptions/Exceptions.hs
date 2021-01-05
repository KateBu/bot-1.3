module Exceptions.Exceptions where

import qualified Data.Text as T
import Control.Exception ( Exception, IOException ) 
import qualified Logger.Logger as Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs

data BotException = InitConfigExcept Logger.LogMessage 
    | IOExept IOException
    | ParseExcept Logger.LogMessage
    | UpdateExcept Logger.LogMessage
    | SendExcept Logger.LogMessage 
    | OtherExcept Logger.LogMessage 
    deriving Eq 
    
instance Show BotException where 
    show (InitConfigExcept logMsg) = 
        "InitConfigException occured -- " <> show logMsg
    show (IOExept ioEx) =
        "IO exception occured -- " <> show ioEx
    show (ParseExcept logMsg) = 
        "Parsing exception occured -- " <> show logMsg 
    show (UpdateExcept logMsg) = 
        "Update exception occured -- " <> show logMsg 
    show (SendExcept logMsg) = 
        "Send exception occured -- " <> show logMsg 
    show (OtherExcept oEx) = 
        "Other exception occured -- " <> show oEx     

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

throwUpdateExcept :: Logger.LogMessage -> Either BotException a
throwUpdateExcept = Left . UpdateExcept  

throwSendExcept :: Logger.LogMessage -> Either BotException a
throwSendExcept = Left . SendExcept  

throwIOException :: Monad m => IOException -> m (Either BotException a)
throwIOException = pure . Left . IOExept 

throwOtherException :: Logger.LogMessage -> Either BotException a
throwOtherException = Left . OtherExcept 

catchBotExcept :: Monad m => (BotException -> Either BotException a)
    -> Either BotException a 
    -> m (Either BotException a) 
catchBotExcept _ (Right smth) = pure $ Right smth 
catchBotExcept handler (Left exception) = 
    pure $ handler exception 





