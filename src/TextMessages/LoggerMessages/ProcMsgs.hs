module TextMessages.LoggerMessages.ProcMsgs where

import qualified Environment.Logger.Structs as Logger

emptyMsgProcessingInProgress :: Logger.LogMessage
emptyMsgProcessingInProgress = Logger.LogMessage Logger.Debug "Empty Message processing in progress..."

helpCommandProcessingInProgress :: Logger.LogMessage
helpCommandProcessingInProgress = Logger.LogMessage Logger.Debug "Help Command processing in progress..."

sendMsgInProgress :: Logger.LogMessage
sendMsgInProgress = Logger.LogMessage Logger.Debug "sendMessage function in progress..."

getApiResponseInProgress :: Logger.LogMessage
getApiResponseInProgress = Logger.LogMessage Logger.Debug "trying to get API response..."

repeatCommandProcessingInProgress :: Logger.LogMessage
repeatCommandProcessingInProgress = Logger.LogMessage Logger.Debug "Repeat Command processing in progress..."

callbackMsgProcessingInProgress :: Logger.LogMessage
callbackMsgProcessingInProgress = Logger.LogMessage Logger.Debug "Callback processing in progress..."
