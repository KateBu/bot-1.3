module API.VK.Functions.MsgTypes.TextMsg where

import qualified API.VK.Structs.Exports as VK
import qualified Logic.Structs as PureStructs

buildTextMessage :: VK.Message -> Maybe PureStructs.MessageType
buildTextMessage msg = case VK.msg_text msg of
  Nothing -> Nothing
  Just "" -> Nothing
  _ -> pure $ PureStructs.MsgTypeCommon "Message"
