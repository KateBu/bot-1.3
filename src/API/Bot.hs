module API.Bot where

import Control.Exception (catch)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Environment.Internals as Env
import qualified Environment.Logger.Internals as Logger
import qualified Environment.Logger.Messages as LoggerMsgs
import qualified Exceptions.Internals as BotEx
import qualified Logic.Logic as Logic
import qualified Services.SHandle as Handle

runBot :: Env.Environment IO -> IO ()
runBot env =
  catch
    ( do
        let handle = Handle.new env
        Handle.getUpdates handle
          >>= Logic.processMsgs env handle
          >>= nextLoop
    )
    BotEx.handleBotException

nextLoop :: Env.Environment IO -> IO ()
nextLoop env = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.nextLoop >> runBot env
