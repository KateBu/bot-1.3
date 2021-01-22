module Environment.Logger.Messages.ProcMsgs where

import qualified Environment.Logger.LoggerStructs as Logger

emptyMsg :: Logger.LogMessage
emptyMsg = Logger.LogMessage Logger.Debug "Empty Message processing in progress..."

helpCmd :: Logger.LogMessage
helpCmd = Logger.LogMessage Logger.Debug "Help Command processing in progress..."

sendMsg :: Logger.LogMessage
sendMsg = Logger.LogMessage Logger.Debug "sendM function in progress..."

getApiResp :: Logger.LogMessage
getApiResp = Logger.LogMessage Logger.Debug "trying to get API response..."

repeatCmd :: Logger.LogMessage
repeatCmd = Logger.LogMessage Logger.Debug "Repeat Command processing in progress..."

callbackMsg :: Logger.LogMessage
callbackMsg = Logger.LogMessage Logger.Debug "Callback processing in progress..."
