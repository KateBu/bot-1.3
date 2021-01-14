module Handle.Handle where

import qualified API.Wrapper as Wrapper
import qualified Config.Config as Config
import qualified Logger.Logger as Logger
import qualified Logic.PureStructs as PureStructs
import qualified Environment.Environment as Env 

data Handle' m = Handle'
  { hConfig' :: m Config.Config,
  --  hLogger' :: m (Logger.Logger m),
    hGetUpdates' ::
      Env.Env m ->
      m [PureStructs.PureMessage],
    hSendMessage' ::
      Env.Env m ->
      PureStructs.PureMessage ->
      m (Env.Env m)
  }

new' :: Config.Config -> IO (Handle' IO)
new' config =
  pure $
    Handle'
      { hConfig' = pure config,
       -- hLogger' = Logger.createLogger (Config.priority config),
        hGetUpdates' = Wrapper.getPureMessageList',
        hSendMessage' = Wrapper.sendM' 
      }

data Handle m = Handle
  { hConfig :: m Config.Config,
    hLogger :: m (Logger.Logger m),
    hGetUpdates ::
      Config.Config ->
      Logger.Logger m ->
      m [PureStructs.PureMessage],
    hSendMessage ::
      Config.Config ->
      Logger.Logger m ->
      PureStructs.PureMessage ->
      m Config.Config
  }

new :: Config.Config -> IO (Handle IO)
new config =
  pure $
    Handle
      { hConfig = pure config,
        hLogger = Logger.createLogger (Config.priority config),
        hGetUpdates = Wrapper.getPureMessageList,
        hSendMessage = Wrapper.sendM
      }
