module API.Bot where

import Control.Exception (catch)
import qualified Environment.Environment as Env
import qualified Exceptions.Exceptions as BotEx
--import qualified Handle.Handle as Handle
import qualified Environment.Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.Logic as Logic

runBot :: Env.Environment IO -> IO ()
runBot env = undefined 
 {- catch
    ( do
        currentConf <- Env.eGetConfig env
        handle <- Handle.new currentConf
        Handle.hGetUpdates handle env
          >>= Logic.processMsgs env (Handle.hSendMessage handle)
          >>= nextLoop
    )
    BotEx.handleBotException-}

nextLoop :: Env.Environment IO -> IO ()
nextLoop env = undefined
{- do
  Env.eLog LoggerMsgs.nextLoop env >> runBot env
-}