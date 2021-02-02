module Services.DB.Database.Functions where

import qualified Config.Internals as Config
import Control.Exception (catch, catches)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( ConnectInfo (connectDatabase, connectPassword, connectUser),
    Only (Only),
    SqlError,
    connect,
    defaultConnectInfo,
    query,
    query_,
  )
import qualified Environment.Internals as Env
import qualified Environment.Logger.Internals as Logger
import qualified TextMessages.LoggerMessages as LoggerMsgs
import qualified Exceptions.Internals as BotEx
import qualified Logic.PureStructs as PureStructs
import Services.DB.Database.Queries
  ( addUserQuery,
    findUserQuery,
    updateUserQuery,
    userIdText,
  )

connectInfo :: ConnectInfo
connectInfo =
  defaultConnectInfo
    { connectUser = "bot",
      connectPassword = "bot13",
      connectDatabase = "users"
    }

find :: Config.Config -> Logger.Logger IO -> PureStructs.ChatID -> IO (Maybe Env.RepeatNumber)
find bType logger userId = do
  conn <- connect connectInfo `catch` \ex -> BotEx.throwSQLException (ex :: SqlError)
  resp <- query_ conn (findUserQuery bType userId) `catches` BotEx.dbErrorsHandlers
  checkFindResponse logger resp

checkFindResponse :: Monad m => Logger.Logger m -> [Only (Maybe Env.RepeatNumber)] -> m (Maybe Env.RepeatNumber)
checkFindResponse _ [] = pure Nothing
checkFindResponse logger [Only rep] = do
  Logger.botLog logger LoggerMsgs.findUserScs
  pure rep
checkFindResponse _ _ = BotEx.throwOtherException LoggerMsgs.findUserQueryFld

add :: Config.Config -> Logger.Logger IO -> PureStructs.ChatID -> Env.RepeatNumber -> IO ()
add bType logger usId rep = do
  conn <- connect connectInfo `catch` \ex -> BotEx.throwSQLException (ex :: SqlError)
  let user = userIdText bType usId
  resp <- (query conn addUserQuery (user, rep) :: IO [(T.Text, Env.RepeatNumber)]) `catches` BotEx.dbErrorsHandlers
  checkAddResp logger (user, rep) resp

checkAddResp ::
  Monad m =>
  Logger.Logger m ->
  (T.Text, Env.RepeatNumber) ->
  [(T.Text, Env.RepeatNumber)] ->
  m ()
checkAddResp logger (usId, rep) [(usId', rep')] = do
  if usId `T.isPrefixOf` usId' && rep == rep'
    then Logger.botLog logger LoggerMsgs.addUserRepeatScs
    else BotEx.throwOtherException LoggerMsgs.addUserQueryFld
checkAddResp _ _ _ = BotEx.throwOtherException LoggerMsgs.addUserQueryFld

update :: Config.Config -> Logger.Logger IO -> PureStructs.ChatID -> Env.RepeatNumber -> IO ()
update bType logger usId rep = do
  conn <- connect connectInfo `catch` \ex -> BotEx.throwSQLException (ex :: SqlError)
  let user = userIdText bType usId
  resp <-
    (query conn updateUserQuery (rep, user) :: IO [(T.Text, Env.RepeatNumber)])
      `catches` BotEx.dbErrorsHandlers
  checkUpdResp logger (user, rep) resp

checkUpdResp ::
  Monad m =>
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
