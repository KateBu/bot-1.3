module Environment.Logger.Messages.Bot where

import qualified Environment.Logger.LoggerStructs as Logger

vkFatalError :: Logger.LogMessage
vkFatalError =
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

initLogFld :: Logger.LogMessage
initLogFld = Logger.LogMessage Logger.Error "Cannot parse log priority from config file"

getUpdFld :: Logger.LogMessage
getUpdFld =
  Logger.LogMessage
    Logger.Error
    "GetUpdates function failed: "

readValueFld :: Logger.LogMessage
readValueFld =
  Logger.LogMessage
    Logger.Error
    "Cannot read value"

nextLoop :: Logger.LogMessage
nextLoop =
  Logger.LogMessage
    Logger.Debug
    "Go to the next loop..."
