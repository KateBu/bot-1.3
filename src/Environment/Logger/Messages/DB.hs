module Environment.Logger.Messages.DB where

import qualified Environment.Logger.LoggerStructs as Logger

addUserQueryFld :: Logger.LogMessage
addUserQueryFld = Logger.LogMessage Logger.Error "Impossible to insert User into Database"

findUserQueryFld :: Logger.LogMessage
findUserQueryFld = Logger.LogMessage Logger.Error "Impossible to find User in Database"

updUserRepeatFld :: Logger.LogMessage
updUserRepeatFld = Logger.LogMessage Logger.Error "Impossible to update User repeat in Database"

updUserRepeatNoUser :: Logger.LogMessage
updUserRepeatNoUser = Logger.LogMessage Logger.Warning "Impossible to find userId in Database to update repeats"

addUserRepeatScs :: Logger.LogMessage
addUserRepeatScs = Logger.LogMessage Logger.Info "New User was inserted into Database"

updUserRepeatScs :: Logger.LogMessage
updUserRepeatScs = Logger.LogMessage Logger.Info "New repeat value for User was set in Database"

findUserScs :: Logger.LogMessage
findUserScs = Logger.LogMessage Logger.Debug "User was found in Database..."
