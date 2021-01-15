module Handle.Handle where

import qualified API.Wrapper as Wrapper
import qualified Config.Config as Config
import qualified Environment.Environment as Env
import qualified Logic.PureStructs as PureStructs

data Handle m = Handle
  { hConfig :: m Config.Config,
    hGetUpdates ::
      Env.Env m ->
      m [PureStructs.PureMessage],
    hSendMessage ::
      Env.Env m ->
      PureStructs.PureMessage ->
      m (Env.Env m)
  }

new :: Config.Config -> IO (Handle IO)
new config =
  pure $
    Handle
      { hConfig = pure config,
        hGetUpdates = Wrapper.getPureMessageList,
        hSendMessage = Wrapper.sendM
      }
