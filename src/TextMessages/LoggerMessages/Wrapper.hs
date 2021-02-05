module TextMessages.LoggerMessages.Wrapper where

import qualified Environment.Logger.Structs as Logger

sendMsgFailed :: Logger.LogMessage
sendMsgFailed =
  Logger.LogMessage
    Logger.Error
    "sendMessage function failed: "

invalidHP :: Logger.LogMessage
invalidHP =
  Logger.LogMessage
    Logger.Error
    "Invalid host path"

badServerResponse :: Logger.LogMessage
badServerResponse =
  Logger.LogMessage
    Logger.Error
    "Got bad server response, response status code: "

getUpdatesInProcess :: Logger.LogMessage
getUpdatesInProcess =
  Logger.LogMessage
    Logger.Debug
    "getU function: trying to get updates..."

getResponseMultipartInProgress :: Logger.LogMessage
getResponseMultipartInProgress =
  Logger.LogMessage
    Logger.Debug
    "Trying to get response with multipart body..."

getResponseUrlInProgress :: Logger.LogMessage
getResponseUrlInProgress =
  Logger.LogMessage
    Logger.Debug
    "Trying to get response with url-encoded body..."
