module Services.DBHandle.DBHandle where

import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Environment.Environment as Env
import qualified Services.DBHandle.Database.Database as DB

data Handle m = Handle
  { findUser :: Int -> m (Maybe Int),
    addUser :: Int -> Int -> m (),
    updateUser :: Int -> Int -> m ()
  }

new :: Env.Environment IO -> IO (Handle IO)
new env = do
  config <- runReaderT Env.eConfig env
  logger <- runReaderT Env.eLogger env
  pure $
    Handle
      { findUser = DB.find config logger,
        addUser = DB.add config logger,
        updateUser = DB.update config logger
      }
