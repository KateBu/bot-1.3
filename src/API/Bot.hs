module API.Bot where

import qualified Control.Exception as Ex
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Environment.Exports as Env
import qualified Environment.Logger.Exports as Logger
import qualified Exceptions.Exports as BotEx
import qualified Logic.Main as Logic
import qualified Services.Exports as Services
import qualified TextMessages.LoggerMessages as LoggerMsgs

runBot :: Env.Environment IO -> IO ()
runBot env =
  withBotExceptionWrapped $
    Services.getUpdates env >>= Logic.processMsgs env >>= nextLoop

nextLoop :: Env.Environment IO -> IO ()
nextLoop env = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.nextLoop >> runBot env

withBotExceptionWrapped :: IO () -> IO ()
withBotExceptionWrapped = Ex.handle BotEx.handleBotException
