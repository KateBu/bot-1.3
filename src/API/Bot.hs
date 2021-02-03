module API.Bot where

import Control.Exception ( handle )
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Environment.Internals as Env
import qualified Environment.Logger.Internals as Logger
import qualified Exceptions.Internals as BotEx
import qualified Logic.Main as Logic
import qualified Services.SHandle as Handle
import qualified TextMessages.LoggerMessages as LoggerMsgs

runBot :: Env.Environment IO -> IO ()
runBot env =
  withBotExceptionWrapped $
    do
      let hdl = Handle.new env
      Handle.getUpdates hdl
        >>= Logic.processMsgs env hdl
        >>= nextLoop

nextLoop :: Env.Environment IO -> IO ()
nextLoop env = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.nextLoop >> runBot env

withBotExceptionWrapped :: IO () -> IO ()
withBotExceptionWrapped = handle BotEx.handleBotException
