module Services.API.Handle where

import qualified API.Wrapper as Wrapper
import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs

data Handle m = Handle
  { hGetUpdates ::
      m [PureStructs.PureMessage],
    hSendMessage ::
      PureStructs.PureMessage ->
      m (Env.Environment m)
  }

new :: Env.Environment IO -> IO (Handle IO)
new env =
  pure $
    Handle
      { hGetUpdates = Wrapper.getPureMessageList env,
        hSendMessage = Wrapper.sendMessage env
      }
