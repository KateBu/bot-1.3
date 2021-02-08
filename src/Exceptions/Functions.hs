module Exceptions.Functions where

import Control.Exception
  ( Handler (..),
    IOException,
    throwIO,
  )
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( FormatError,
    ResultError,
    SqlError,
  )
import qualified Environment.Logger.Exports as Logger
import Exceptions.Structs (BotException (..))
import Network.HTTP.Req (HttpException)
import qualified TextMessages.LoggerMessages as LoggerMsgs

class (Monad m) => MonadThrow m where
  throwBotExcept :: BotException -> m a

instance MonadThrow IO where
  throwBotExcept = throwIO

instance MonadThrow Maybe where
  throwBotExcept = const Nothing

handleBotException :: BotException -> IO ()
handleBotException exception = do
  print exception
  putStrLn "Program terminated"

throwInitConfigExcept :: MonadThrow m => m a
throwInitConfigExcept = throwBotExcept $ InitConfigExcept LoggerMsgs.initConfigExcept

throwInitConfigExceptWithMessage :: MonadThrow m => Logger.LogMessage -> m a
throwInitConfigExceptWithMessage logMsg = throwBotExcept $ InitConfigExcept logMsg

throwParseExcept :: MonadThrow m => String -> m a
throwParseExcept err = throwBotExcept $ ParseExcept (Logger.makeLogMessage LoggerMsgs.parseError (T.pack err))

throwUpdateExcept :: MonadThrow m => Logger.LogMessage -> m a
throwUpdateExcept = throwBotExcept . UpdateExcept

throwSendExcept :: MonadThrow m => Logger.LogMessage -> m a
throwSendExcept = throwBotExcept . SendExcept

throwIOException :: MonadThrow m => IOException -> m a
throwIOException = throwBotExcept . IOExcept

throwOtherException :: MonadThrow m => Logger.LogMessage -> m a
throwOtherException = throwBotExcept . OtherExcept

throwHttpException :: MonadThrow m => HttpException -> m a
throwHttpException err = throwBotExcept . HttpExcept $ logMsg
  where
    logMsg = Logger.makeLogMessage LoggerMsgs.httpException (T.pack . show $ err)

throwSQLException :: MonadThrow m => SqlError -> m a
throwSQLException = throwBotExcept . DBSqlError

throwDBFormatExceptions :: MonadThrow m => FormatError -> m a
throwDBFormatExceptions = throwBotExcept . DBFormatError

throwDBResultError :: MonadThrow m => ResultError -> m a
throwDBResultError = throwBotExcept . DBResultError

dbErrorsHandlers :: [Handler a]
dbErrorsHandlers =
  [ Handler $ \sqlErr -> throwSQLException (sqlErr :: SqlError),
    Handler $ \formatErr -> throwDBFormatExceptions (formatErr :: FormatError),
    Handler $ \resultErr -> throwDBResultError (resultErr :: ResultError)
  ]
