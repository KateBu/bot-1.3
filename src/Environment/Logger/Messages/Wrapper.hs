module Environment.Logger.Messages.Wrapper where

import qualified Environment.Logger.Structs as Logger

httpEx :: Logger.LogMessage
httpEx = Logger.LogMessage Logger.Error "Http exception: "

sndMsgFld :: Logger.LogMessage
sndMsgFld =
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

noUpd :: Logger.LogMessage
noUpd =
  Logger.LogMessage
    Logger.Error
    "No updates were found"

parseErr :: Logger.LogMessage
parseErr = Logger.LogMessage Logger.Error "Parsing bytestring into struct failed: "

getUpdInProcess :: Logger.LogMessage
getUpdInProcess =
  Logger.LogMessage
    Logger.Debug
    "getU function: trying to get updates..."

getRespMulti :: Logger.LogMessage
getRespMulti =
  Logger.LogMessage
    Logger.Debug
    "Trying to get response with multipart body..."

getRespUrl :: Logger.LogMessage
getRespUrl =
  Logger.LogMessage
    Logger.Debug
    "Trying to get response with url-encoded body..."
