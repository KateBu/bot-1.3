module Exceptions.Functions where

import Control.Exception (Handler (..), IOException, throw)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( FormatError,
    ResultError,
    SqlError,
  )
import qualified Environment.Logger.Internals as Logger
import Exceptions.Structs (BotException (..))
import Network.HTTP.Req (HttpException)
import qualified TextMessages.LoggerMessages as LoggerMsgs

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

throwSQLException :: Monad m => SqlError -> m a
throwSQLException = pure . throw . DBSqlError

throwDBFormatExceptions :: Monad m => FormatError -> m a
throwDBFormatExceptions = pure . throw . DBFormatError

throwDBResultError :: Monad m => ResultError -> m a
throwDBResultError = pure . throw . DBResultError

dbErrorsHandlers :: [Handler a]
dbErrorsHandlers =
  [ Handler $ \sqlErr -> throwSQLException (sqlErr :: SqlError),
    Handler $ \fErr -> throwDBFormatExceptions (fErr :: FormatError),
    Handler $ \rErr -> throwDBResultError (rErr :: ResultError)
  ]
