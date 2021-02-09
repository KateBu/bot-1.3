module API.Telegram.URL where

import qualified Environment.Exports as Env
import qualified Logic.Structs as PureStructs
import qualified Wrapper.Structs as WrapStructs

buildGetUpdateHostPath :: Env.Config -> Maybe WrapStructs.HostPath
buildGetUpdateHostPath (Env.TBot (Env.Telegram tok _)) =
  pure $
    WrapStructs.HostPath "api.telegram.org" ["bot" <> tok, "getUpdates"]
buildGetUpdateHostPath _ = Nothing

buildGetUpdateParams :: Env.Config -> [PureStructs.Params]
buildGetUpdateParams (Env.TBot (Env.Telegram _ offset)) =
  [ PureStructs.ParamsNum "offset" offset,
    PureStructs.ParamsText "timeout" Env.timeOut
  ]
buildGetUpdateParams _ = []

buildSendHostPath :: Env.Config -> PureStructs.MessageType -> Maybe WrapStructs.HostPath
buildSendHostPath (Env.TBot (Env.Telegram tok _)) (PureStructs.MsgTypeUserCommand PureStructs.Help) =
  pure $
    WrapStructs.HostPath
      "api.telegram.org"
      [ "bot" <> tok,
        "sendMessage"
      ]
buildSendHostPath (Env.TBot (Env.Telegram tok _)) (PureStructs.MsgTypeUserCommand PureStructs.Repeat) =
  pure $
    WrapStructs.HostPath
      "api.telegram.org"
      [ "bot" <> tok,
        "sendMessage"
      ]
buildSendHostPath (Env.TBot (Env.Telegram tok _)) (PureStructs.MsgTypeCommon mType) =
  pure $
    WrapStructs.HostPath
      "api.telegram.org"
      [ "bot" <> tok,
        "send" <> mType
      ]
buildSendHostPath _ _ = Nothing
