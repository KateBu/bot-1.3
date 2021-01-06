module API.Bot where

import qualified Config.Config as Config
import qualified Exceptions.Exceptions as BotEx
import qualified Handle.Handle as Handle
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.Logic as Logic

runBot :: Either BotEx.BotException Config.Config -> IO ()
runBot (Left err) = BotEx.handleBotException err
runBot (Right config) = do
  handle <- Handle.new config
  logger <- Handle.hLogger handle
  conf <- Handle.hConfig handle
  Handle.hGetUpdates handle conf logger
    >>= Logic.processMsgs config logger (Handle.hSendMessage handle)
    >>= nextLoop logger

nextLoop :: Logger.Logger -> (Either BotEx.BotException Config.Config) -> IO ()
nextLoop logger config = do
  Logger.botLog logger LoggerMsgs.nextLoop
  runBot config
