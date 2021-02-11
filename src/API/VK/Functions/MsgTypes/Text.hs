module API.VK.Functions.MsgTypes.Text where

import qualified API.PureStructs.Exports as PureStructs
import qualified API.VK.Structs.Exports as VK

buildTextMessage :: VK.Message -> Maybe PureStructs.MessageType
buildTextMessage msg = case VK.msg_text msg of
  Nothing -> Nothing
  Just "" -> Nothing
  _ -> pure $ PureStructs.MsgTypeCommon "Message"
