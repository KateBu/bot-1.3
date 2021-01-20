module Services.DBHandle.Database.Database where

import Database.PostgreSQL.Simple
    ( SqlError,
      query,
      query_,
      connect,
      defaultConnectInfo,
      Only(Only),
      ConnectInfo(connectUser, connectPassword, connectDatabase) ) 
import qualified Data.Text as T 
import Control.Exception ( catches, catch )
import qualified Config.Config as Config 
import Services.DBHandle.Database.Queries
    ( userIdText, findUserQuery, addUserQuery, updateUserQuery )
import qualified Exceptions.Exceptions as BotEx 
import qualified Environment.Logger.Logger as Logger
import qualified Environment.Logger.LoggerMsgs as LoggerMsgs
   
   

connectInfo :: ConnectInfo 
connectInfo = defaultConnectInfo 
    {
        connectUser = "bot",
        connectPassword = "bot13",
        connectDatabase = "users"
    }

find :: Config.Config -> Logger.Logger IO -> Int -> IO (Maybe Int) 
find bType logger userId = do 
    conn <- connect connectInfo `catch` \ex -> BotEx.throwSQLException (ex :: SqlError) 
    resp <- query_ conn (findUserQuery bType userId) `catches` BotEx.dbErrorsHandlers
    checkFindResponse logger resp 

checkFindResponse :: Monad m =>  Logger.Logger m -> [Only (Maybe Int)] -> m (Maybe Int)
checkFindResponse _ [] = pure Nothing  
checkFindResponse logger [Only repeat] = do 
    Logger.botLog logger LoggerMsgs.findUserScs 
    pure repeat 
checkFindResponse _ _ = BotEx.throwOtherException LoggerMsgs.findUserQueryFld

add :: Config.Config -> Logger.Logger IO -> Int -> Int -> IO ()  
add bType logger usId repeat = do 
    conn <- connect connectInfo `catch` \ex -> BotEx.throwSQLException (ex :: SqlError) 
    let user = userIdText bType usId
    resp  <- (query conn addUserQuery (user, repeat) :: IO [(T.Text, Int)]) `catches` BotEx.dbErrorsHandlers
    checkAddResp logger (user,repeat) resp  

checkAddResp :: Monad m => Logger.Logger m -> (T.Text, Int) -> [(T.Text, Int)] -> m () 
checkAddResp logger usRep [usRep'] = if usRep == usRep' 
    then Logger.botLog logger LoggerMsgs.addUserRepeatScs
    else BotEx.throwOtherException LoggerMsgs.addUserQueryFld

update :: Config.Config -> Logger.Logger IO -> Int -> Int -> IO ()  
update bType logger usId repeat = do 
    conn <- connect connectInfo `catch` \ex -> BotEx.throwSQLException (ex :: SqlError) 
    let user = userIdText bType usId
    resp  <- (query conn updateUserQuery (repeat, user) :: IO [(T.Text, Int)]) `catches` BotEx.dbErrorsHandlers
    checkUpdResp logger (user,repeat) resp  

checkUpdResp :: Monad m => Logger.Logger m -> (T.Text, Int) -> [(T.Text, Int)] -> m ()  
checkUpdResp logger _ [] = 
    Logger.botLog logger LoggerMsgs.updUserRepeatNoUser 
checkUpdResp logger usRep [usRep'] = if usRep == usRep' 
    then Logger.botLog logger LoggerMsgs.updUserRepeatScs
    else 
        BotEx.throwOtherException LoggerMsgs.updUserRepeatFld


