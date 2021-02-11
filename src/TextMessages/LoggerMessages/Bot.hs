module TextMessages.LoggerMessages.Bot where

import qualified Logger.Structs as Logger

vkSettingsFatalError :: Logger.LogMessage
vkSettingsFatalError =
  Logger.LogMessage
    Logger.Error
    "FATAL ERROR: VK Config parsing error. Couldn't find group id or token"

initConfigExcept :: Logger.LogMessage
initConfigExcept =
  Logger.LogMessage
    Logger.Error
    ( "Couldn't find or read the config file,"
        <> " check the path to the file and the information inside it"
    )

initLogFailed :: Logger.LogMessage
initLogFailed = Logger.LogMessage Logger.Error "Cannot parse log priority from config file"

getUpdateFailed :: Logger.LogMessage
getUpdateFailed =
  Logger.LogMessage
    Logger.Error
    "GetUpdates function failed: "

readUpdateIdFailed :: Logger.LogMessage
readUpdateIdFailed =
  Logger.LogMessage
    Logger.Error
    "Cannot read value"

nextLoop :: Logger.LogMessage
nextLoop =
  Logger.LogMessage
    Logger.Debug
    "Go to the next loop..."
