module TextMessages.LoggerMessages.Exceptions where

import qualified Logger.Structs as Logger

parseError :: Logger.LogMessage
parseError = Logger.LogMessage Logger.Error "Parsing bytestring into struct failed: "

httpException :: Logger.LogMessage
httpException = Logger.LogMessage Logger.Error "Http exception: "
