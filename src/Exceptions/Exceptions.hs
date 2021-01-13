module Exceptions.Exceptions where

import Control.Exception (Exception, IOException, throw)
import qualified Data.Text as T
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import Network.HTTP.Req (HttpException)

data BotException
  = InitConfigExcept Logger.LogMessage
  | IOExcept IOException
  | ParseExcept Logger.LogMessage
  | UpdateExcept Logger.LogMessage
  | SendExcept Logger.LogMessage
  | HttpExcept Logger.LogMessage
  | OtherExcept Logger.LogMessage
  deriving (Eq)

instance Show BotException where
  show (InitConfigExcept logMsg) =
    "InitConfigException occured -- " <> show logMsg
  show (IOExcept ioEx) =
    "IO exception occured -- " <> show ioEx
  show (ParseExcept logMsg) =
    "Parsing exception occured -- " <> show logMsg
  show (UpdateExcept logMsg) =
    "Update exception occured -- " <> show logMsg
  show (SendExcept logMsg) =
    "Send exception occured -- " <> show logMsg
  show (HttpExcept logMsg) =
    "Http exception occured -- " <> show logMsg
  show (OtherExcept oEx) =
    "Other exception occured -- " <> show oEx

instance Exception BotException

handleBotException :: BotException -> IO ()
handleBotException ex = do
  print ex
  putStrLn "Program terminated"

throwBotExcept :: Monad m => BotException -> m a
throwBotExcept = pure . throw

throwInitConfigExcept :: Monad m => m a
throwInitConfigExcept = pure . throw $ InitConfigExcept LoggerMsgs.initConfigExcept

throwParseExcept :: Monad m => String -> m a
throwParseExcept err = throwBotExcept $ ParseExcept (Logger.makeLogMessage LoggerMsgs.parseErr (T.pack err))

throwUpdateExcept :: Monad m => Logger.LogMessage -> m a
throwUpdateExcept = pure . throw . UpdateExcept

throwPureUpdateExcept :: Logger.LogMessage -> a
throwPureUpdateExcept = throw . UpdateExcept

throwSendExcept :: Monad m => Logger.LogMessage -> m a
throwSendExcept = pure . throw . SendExcept

throwIOException :: Monad m => IOException -> m a
throwIOException = pure . throw . IOExcept

throwOtherException :: Monad m => Logger.LogMessage -> m a
throwOtherException = pure . throw . OtherExcept

throwPureOtherException :: Logger.LogMessage -> a
throwPureOtherException = throw . OtherExcept

throwHttpException :: Monad m => HttpException -> m a
throwHttpException err = pure . throw . HttpExcept $ Logger.makeLogMessage LoggerMsgs.httpEx (T.pack . show $ err)
