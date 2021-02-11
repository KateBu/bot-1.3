module Wrapper.URL.Telegram where

import qualified API.PureStructs.Exports as PureStructs
import qualified Config.Exports as Config
import qualified Wrapper.URL.Structs as URLStructs

buildGetUpdateHostPath :: Config.Config -> Maybe URLStructs.HostPath
buildGetUpdateHostPath (Config.TBot (Config.Telegram tok _)) =
  pure $
    URLStructs.HostPath "api.telegram.org" ["bot" <> tok, "getUpdates"]
buildGetUpdateHostPath _ = Nothing

buildGetUpdateParams :: Config.Config -> [PureStructs.Params]
buildGetUpdateParams (Config.TBot (Config.Telegram _ offset)) =
  [ PureStructs.ParamsNum "offset" offset,
    PureStructs.ParamsText "timeout" Config.timeOut
  ]
buildGetUpdateParams _ = []

buildSendHostPath :: Config.Config -> PureStructs.MessageType -> Maybe URLStructs.HostPath
buildSendHostPath (Config.TBot (Config.Telegram tok _)) (PureStructs.MsgTypeUserCommand PureStructs.Help) =
  pure $
    URLStructs.HostPath
      "api.telegram.org"
      [ "bot" <> tok,
        "sendMessage"
      ]
buildSendHostPath (Config.TBot (Config.Telegram tok _)) (PureStructs.MsgTypeUserCommand PureStructs.Repeat) =
  pure $
    URLStructs.HostPath
      "api.telegram.org"
      [ "bot" <> tok,
        "sendMessage"
      ]
buildSendHostPath (Config.TBot (Config.Telegram tok _)) (PureStructs.MsgTypeCommon mType) =
  pure $
    URLStructs.HostPath
      "api.telegram.org"
      [ "bot" <> tok,
        "send" <> mType
      ]
buildSendHostPath _ _ = Nothing
