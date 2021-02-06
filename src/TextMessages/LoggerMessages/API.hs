module TextMessages.LoggerMessages.API where

import qualified Environment.Logger.Structs as Logger

apiHandleCloseMsg :: Logger.LogMessage
apiHandleCloseMsg =
  Logger.LogMessage
    Logger.Debug
    "API handle closed"

apiHandleCreateMsg :: Logger.LogMessage
apiHandleCreateMsg =
  Logger.LogMessage
    Logger.Debug
    "API handle created"
