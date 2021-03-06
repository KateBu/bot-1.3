module Services.DB.Handle (withDBHandle, Handle (..)) where

import qualified API.PureStructs.Exports as PureStructs
import Control.Exception (bracket)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Environment.Exports as Env
import qualified Logger.Exports as Logger
import qualified Services.DB.Database.Functions as DB
import qualified TextMessages.LoggerMessages as LoggerMsgs

data Handle m = Handle
  { findUser :: PureStructs.ChatID -> m (Maybe Env.RepeatNumber),
    addUser :: PureStructs.ChatID -> Env.RepeatNumber -> m (),
    updateUser :: PureStructs.ChatID -> Env.RepeatNumber -> m ()
  }

new :: Env.Environment IO -> IO (Handle IO)
new env = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.dbHandleCreateMsg
  pure $
    Handle
      { findUser = DB.find env,
        addUser = DB.add env,
        updateUser = DB.update env
      }

close :: Env.Environment IO -> Handle IO -> IO ()
close env _ = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.dbHandleCloseMsg

withDBHandle :: Env.Environment IO -> (Handle IO -> IO a) -> IO a
withDBHandle env = bracket (new env) (close env)
