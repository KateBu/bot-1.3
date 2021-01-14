module API.Bot where

import qualified Config.Config as Config
import Control.Exception (catch)
import Control.Monad.Reader ( ReaderT(runReaderT) ) 
import qualified Exceptions.Exceptions as BotEx
import qualified Handle.Handle as Handle
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.Logic as Logic
import qualified Environment.Environment as Env 

runBot :: Env.Env IO -> IO ()
runBot env =
  catch
    ( do
        currentConf <- runReaderT Env.eGetConfig env
        handle <- Handle.new' currentConf
        Handle.hGetUpdates' handle env
          >>= Logic.processMsgs' env (Handle.hSendMessage' handle)
          >>= nextLoop
    )
    BotEx.handleBotException

nextLoop :: Env.Env IO -> IO ()
nextLoop env = do
  runReaderT (Env.eLog LoggerMsgs.nextLoop) env >> runBot env
   





runBot' :: Config.Config -> IO ()
runBot' config =
  catch
    ( do
        handle <- Handle.new config
        logger <- Handle.hLogger handle
        conf <- Handle.hConfig handle
        Handle.hGetUpdates handle conf logger
          >>= Logic.processMsgs config logger (Handle.hSendMessage handle)
          >>= nextLoop' logger
    )
    BotEx.handleBotException

nextLoop' :: Logger.Logger IO -> Config.Config -> IO ()
nextLoop' logger config = do
  Logger.botLog logger LoggerMsgs.nextLoop
  runBot' config
