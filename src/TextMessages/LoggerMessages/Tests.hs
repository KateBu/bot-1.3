module TextMessages.LoggerMessages.Tests where

import qualified Environment.Logger.Structs as Logger

testError :: Logger.LogMessage
testError =
  Logger.LogMessage
    Logger.Debug
    "Test error"
