module TextMessages.LoggerMessages.DB where

import qualified Logger.Structs as Logger

addUserQueryFailed :: Logger.LogMessage
addUserQueryFailed = Logger.LogMessage Logger.Error "Impossible to insert User into Database"

findUserQueryFailed :: Logger.LogMessage
findUserQueryFailed = Logger.LogMessage Logger.Error "Impossible to find User in Database"

updateUserRepeatFailed :: Logger.LogMessage
updateUserRepeatFailed = Logger.LogMessage Logger.Error "Impossible to update User repeat in Database"

updateUserRepeatNoUser :: Logger.LogMessage
updateUserRepeatNoUser = Logger.LogMessage Logger.Warning "Impossible to find userId in Database to update repeats"

addUserRepeatSuccess :: Logger.LogMessage
addUserRepeatSuccess = Logger.LogMessage Logger.Info "New User was inserted into Database"

updateUserRepeatSuccess :: Logger.LogMessage
updateUserRepeatSuccess = Logger.LogMessage Logger.Info "New repeat value for User was set in Database"

findUserSuccess :: Logger.LogMessage
findUserSuccess = Logger.LogMessage Logger.Debug "User was found in Database..."

dbHandleCloseMsg :: Logger.LogMessage
dbHandleCloseMsg = Logger.LogMessage Logger.Debug "DB handle closed..."

dbHandleCreateMsg :: Logger.LogMessage
dbHandleCreateMsg = Logger.LogMessage Logger.Debug "DB handle created..."
