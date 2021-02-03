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
import qualified Environment.Internals as Env
import qualified Environment.Logger.Internals as Logger
import qualified Exceptions.Internals as BotEx
import qualified Logic.PureStructs as PureStructs
import Services.DB.Database.Queries
  ( addUserQuery,
    findUserQuery,
    updateUserQuery,
    userIdText,
  )
import qualified TextMessages.LoggerMessages as LoggerMsgs

withDBConnection :: Env.DBConnectString -> (Connection -> IO a) -> IO a
withDBConnection conStr body = withDBExceptionsWrapped $
  bracket initCon close $ \conn -> body conn
  where
    initCon = connectPostgreSQL conStr

find :: Env.Environment IO -> PureStructs.ChatID -> IO (Maybe Env.RepeatNumber)
find env userId = do
  config <- runReaderT Env.eConfig env
  logger <- runReaderT Env.eLogger env
  conStr <- runReaderT Env.eDBConnectionString env
  withDBConnection conStr $
    \conn ->
      do
        resp <- query_ conn (findUserQuery config userId)
        checkFindResponse logger resp

checkFindResponse ::
  BotEx.MonadThrow m =>
  Logger.Logger m ->
  [Only (Maybe Env.RepeatNumber)] ->
  m (Maybe Env.RepeatNumber)
checkFindResponse _ [] = pure Nothing
checkFindResponse logger [Only rep] = do
  Logger.botLog logger LoggerMsgs.findUserScs
  pure rep
checkFindResponse _ _ = BotEx.throwOtherException LoggerMsgs.findUserQueryFld

add :: Env.Environment IO -> PureStructs.ChatID -> Env.RepeatNumber -> IO ()
add env chid rep = do
  config <- runReaderT Env.eConfig env
  logger <- runReaderT Env.eLogger env
  conStr <- runReaderT Env.eDBConnectionString env
  let user = userIdText config chid
  withDBConnection conStr $
    \conn ->
      do
        resp <- query conn addUserQuery (user, rep)
        checkAddResp logger (user, rep) resp

checkAddResp ::
  BotEx.MonadThrow m =>
  Logger.Logger m ->
  (T.Text, Env.RepeatNumber) ->
  [(T.Text, Env.RepeatNumber)] ->
  m ()
checkAddResp logger (usId, rep) [(usId', rep')] = do
  if usId `T.isPrefixOf` usId' && rep == rep'
    then Logger.botLog logger LoggerMsgs.addUserRepeatScs
    else BotEx.throwOtherException LoggerMsgs.addUserQueryFld
checkAddResp _ _ _ = BotEx.throwOtherException LoggerMsgs.addUserQueryFld

update :: Env.Environment IO -> PureStructs.ChatID -> Env.RepeatNumber -> IO ()
update env chid rep = do
  config <- runReaderT Env.eConfig env
  logger <- runReaderT Env.eLogger env
  conStr <- runReaderT Env.eDBConnectionString env
  let user = userIdText config chid
  withDBConnection conStr $
    \conn ->
      do
        resp <- query conn updateUserQuery (rep, user)
        checkUpdResp logger (user, rep) resp

checkUpdResp ::
  BotEx.MonadThrow m =>
  Logger.Logger m ->
  (T.Text, Env.RepeatNumber) ->
  [(T.Text, Env.RepeatNumber)] ->
  m ()
checkUpdResp logger _ [] =
  Logger.botLog logger LoggerMsgs.updUserRepeatNoUser
checkUpdResp logger (usId, rep) [(usId', rep')] =
  if usId `T.isPrefixOf` usId' && rep == rep'
    then Logger.botLog logger LoggerMsgs.updUserRepeatScs
    else BotEx.throwOtherException LoggerMsgs.updUserRepeatFld
checkUpdResp _ _ _ = BotEx.throwOtherException LoggerMsgs.updUserRepeatFld

withDBExceptionsWrapped :: IO a -> IO a
withDBExceptionsWrapped = flip catches BotEx.dbErrorsHandlers
