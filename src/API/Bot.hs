module API.Bot where

import Control.Exception (catch)
import qualified Environment.Environment as Env
import qualified Exceptions.Exceptions as BotEx
import qualified Handle.Handle as Handle
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.Logic as Logic

runBot :: Env.Env IO -> IO ()
runBot env =
  catch
    ( do
        currentConf <- Env.eGetConfig env
        handle <- Handle.new currentConf
        Handle.hGetUpdates handle env
          >>= Logic.processMsgs env (Handle.hSendMessage handle)
          >>= nextLoop
    )
    BotEx.handleBotException

nextLoop :: Env.Env IO -> IO ()
nextLoop env = do
  Env.eLog LoggerMsgs.nextLoop env >> runBot env
