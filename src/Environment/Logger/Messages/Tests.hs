module Environment.Logger.Messages.Tests where

import qualified Environment.Logger.LoggerStructs as Logger

testError :: Logger.LogMessage
testError =
  Logger.LogMessage
    Logger.Debug
    "Test error"
