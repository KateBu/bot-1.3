module API.Telegram.TelData where

import qualified API.Wrapper.WrapStructs as WrapStructs
import qualified Config.Config as Config
import qualified Logic.PureStructs as PureStructs

updateHostPath :: Config.Config -> Maybe WrapStructs.HostPath
updateHostPath (Config.TBot (Config.Telegram tok _)) =
  pure $
    WrapStructs.HostPath "api.telegram.org" ["bot" <> tok, "getUpdates"]
updateHostPath _ = Nothing

updateParams :: Config.Config -> [PureStructs.Params]
updateParams (Config.TBot (Config.Telegram _ offset)) =
  [ PureStructs.ParamsNum "offset" offset,
    PureStructs.ParamsText "timeout" Config.timeOut
  ]
updateParams _ = []

sendHostPath :: Config.Config -> PureStructs.MessageType -> Maybe WrapStructs.HostPath
sendHostPath (Config.TBot (Config.Telegram tok _)) (PureStructs.MTUserCommand PureStructs.Help) =
  pure $
    WrapStructs.HostPath
      "api.telegram.org"
      [ "bot" <> tok,
        "sendMessage"
      ]
sendHostPath (Config.TBot (Config.Telegram tok _)) (PureStructs.MTUserCommand PureStructs.Repeat) =
  pure $
    WrapStructs.HostPath
      "api.telegram.org"
      [ "bot" <> tok,
        "sendMessage"
      ]
sendHostPath (Config.TBot (Config.Telegram tok _)) (PureStructs.MTCommon mType) =
  pure $
    WrapStructs.HostPath
      "api.telegram.org"
      [ "bot" <> tok,
        "send" <> mType
      ]
sendHostPath _ _ = Nothing
