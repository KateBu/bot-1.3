module Services.DB.Handle where

import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Environment.Internals as Env
import qualified Logic.PureStructs as PureStructs
import qualified Services.DB.Database.Functions as DB

data Handle m = Handle
  { findUser :: PureStructs.ChatID -> m (Maybe Env.RepeatNumber),
    addUser :: PureStructs.ChatID -> Env.RepeatNumber -> m (),
    updateUser :: PureStructs.ChatID -> Env.RepeatNumber -> m ()
  }

new :: Env.Environment IO -> IO (Handle IO)
new env = do
  dbConStr <- runReaderT Env.eDBConnectionString env
  pure $
    Handle
      { findUser = DB.find' env dbConStr,
        addUser = DB.add' env dbConStr,
        updateUser = DB.update' env dbConStr
      }
