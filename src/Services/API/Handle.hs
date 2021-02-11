module Services.API.Handle (Handle (..), withApiHandle) where

import qualified API.PureStructs.Exports as PureStructs
import Control.Exception (bracket)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Environment.Exports as Env
import qualified Logger.Exports as Logger
import qualified TextMessages.LoggerMessages as LoggerMsgs
import qualified Wrapper.Main as Wrapper

data Handle m = Handle
  { hGetUpdates ::
      m [PureStructs.PureMessage],
    hSendMessage ::
      PureStructs.PureMessage ->
      m (Env.Environment m)
  }

new :: Env.Environment IO -> IO (Handle IO)
new env = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.apiHandleCreateMsg
  pure $
    Handle
      { hGetUpdates = Wrapper.getUpdates env,
        hSendMessage = Wrapper.sendMessage env
      }

close :: Env.Environment IO -> Handle IO -> IO ()
close env _ = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.apiHandleCloseMsg

withApiHandle :: Env.Environment IO -> (Handle IO -> IO a) -> IO a
withApiHandle env = bracket (new env) (close env)
