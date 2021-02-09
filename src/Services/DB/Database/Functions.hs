module Services.DB.Database.Functions where

import Control.Exception (bracket, catches)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    close,
    connectPostgreSQL,
    query,
    query_,
  )
import qualified Environment.Exports as Env
import qualified Exceptions.Exports as BotEx
import qualified Logic.Structs as PureStructs
import Services.DB.Database.Queries
  ( addUserQuery,
    findUserQuery,
    updateUserQuery,
    userIdText,
  )
import qualified TextMessages.LoggerMessages as LoggerMsgs

withDBConnection :: Env.DBConnectString -> (Connection -> IO a) -> IO a
withDBConnection dbConnectString body = withDBExceptionsWrapped $
  bracket initConnection close $ \conn -> body conn
  where
    initConnection = connectPostgreSQL dbConnectString

find :: Env.Environment IO -> PureStructs.ChatID -> IO (Maybe Env.RepeatNumber)
find env userId = do
  (logger, config, dbConnectString) <- loggerConfigDbString env
  withDBConnection dbConnectString $
    \connection ->
      do
        findResponse <- query_ connection (findUserQuery config userId)
        checkFindResponse logger findResponse

checkFindResponse ::
  BotEx.MonadThrow m =>
  Env.Logger m ->
  [Only (Maybe Env.RepeatNumber)] ->
  m (Maybe Env.RepeatNumber)
checkFindResponse _ [] = pure Nothing
checkFindResponse logger [Only repeatNumber] = do
  Env.botLog logger LoggerMsgs.findUserSuccess
  pure repeatNumber
checkFindResponse _ _ = BotEx.throwOtherException LoggerMsgs.findUserQueryFailed

add :: Env.Environment IO -> PureStructs.ChatID -> Env.RepeatNumber -> IO ()
add env chatId repeatNumber = do
  (logger, config, conStr) <- loggerConfigDbString env
  let user = userIdText config chatId
  withDBConnection conStr $
    \connection ->
      do
        addResponse <- query connection addUserQuery (user, repeatNumber)
        checkAddResponse logger (user, repeatNumber) addResponse

checkAddResponse ::
  BotEx.MonadThrow m =>
  Env.Logger m ->
  (T.Text, Env.RepeatNumber) ->
  [(T.Text, Env.RepeatNumber)] ->
  m ()
checkAddResponse logger (chatId, repeatNumber) [(chatId', repeatNumber')] = do
  if chatId `T.isPrefixOf` chatId' && repeatNumber == repeatNumber'
    then Env.botLog logger LoggerMsgs.addUserRepeatSuccess
    else BotEx.throwOtherException LoggerMsgs.addUserQueryFailed
checkAddResponse _ _ _ = BotEx.throwOtherException LoggerMsgs.addUserQueryFailed

update :: Env.Environment IO -> PureStructs.ChatID -> Env.RepeatNumber -> IO ()
update env chatId repeatNumber = do
  (logger, config, conStr) <- loggerConfigDbString env
  let user = userIdText config chatId
  withDBConnection conStr $
    \connection ->
      do
        resp <- query connection updateUserQuery (repeatNumber, user)
        checkUpdateResponse logger (user, repeatNumber) resp

checkUpdateResponse ::
  BotEx.MonadThrow m =>
  Env.Logger m ->
  (T.Text, Env.RepeatNumber) ->
  [(T.Text, Env.RepeatNumber)] ->
  m ()
checkUpdateResponse logger _ [] =
  Env.botLog logger LoggerMsgs.updateUserRepeatNoUser
checkUpdateResponse logger (chatId, repeatNumber) [(chatId', repeatNumber')] =
  if chatId `T.isPrefixOf` chatId' && repeatNumber == repeatNumber'
    then Env.botLog logger LoggerMsgs.updateUserRepeatSuccess
    else BotEx.throwOtherException LoggerMsgs.updateUserRepeatFailed
checkUpdateResponse _ _ _ = BotEx.throwOtherException LoggerMsgs.updateUserRepeatFailed

withDBExceptionsWrapped :: IO a -> IO a
withDBExceptionsWrapped = flip catches BotEx.dbErrorsHandlers

loggerConfigDbString :: Env.Environment IO -> IO (Env.Logger IO, Env.Config, Env.DBConnectString)
loggerConfigDbString env = do
  config <- runReaderT Env.eConfig env
  logger <- runReaderT Env.eLogger env
  dbConnectString <- runReaderT Env.eDBConnectionString env
  pure (logger, config, dbConnectString)
