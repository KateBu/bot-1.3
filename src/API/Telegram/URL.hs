module API.Telegram.URL where

import qualified Environment.Config.Exports as Config
import qualified Logic.Structs as PureStructs
import qualified Wrapper.Structs as WrapStructs

buildGetUpdateHostPath :: Config.Config -> Maybe WrapStructs.HostPath
buildGetUpdateHostPath (Config.TBot (Config.Telegram tok _)) =
  pure $
    WrapStructs.HostPath "api.telegram.org" ["bot" <> tok, "getUpdates"]
buildGetUpdateHostPath _ = Nothing

buildGetUpdateParams :: Config.Config -> [PureStructs.Params]
buildGetUpdateParams (Config.TBot (Config.Telegram _ offset)) =
  [ PureStructs.ParamsNum "offset" offset,
    PureStructs.ParamsText "timeout" Config.timeOut
  ]
buildGetUpdateParams _ = []

buildSendHostPath :: Config.Config -> PureStructs.MessageType -> Maybe WrapStructs.HostPath
buildSendHostPath (Config.TBot (Config.Telegram tok _)) (PureStructs.MsgTypeUserCommand PureStructs.Help) =
  pure $
    WrapStructs.HostPath
      "api.telegram.org"
      [ "bot" <> tok,
        "sendMessage"
      ]
buildSendHostPath (Config.TBot (Config.Telegram tok _)) (PureStructs.MsgTypeUserCommand PureStructs.Repeat) =
  pure $
    WrapStructs.HostPath
      "api.telegram.org"
      [ "bot" <> tok,
        "sendMessage"
      ]
buildSendHostPath (Config.TBot (Config.Telegram tok _)) (PureStructs.MsgTypeCommon mType) =
  pure $
    WrapStructs.HostPath
      "api.telegram.org"
      [ "bot" <> tok,
        "send" <> mType
      ]
buildSendHostPath _ _ = Nothing
