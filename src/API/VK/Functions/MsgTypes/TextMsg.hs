module API.VK.Functions.MsgTypes.TextMsg where

import qualified API.VK.Structs.Exports as VKStructs
import qualified Logic.Structs as PureStructs

buildTextMessage :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
buildTextMessage vkMsg = case VKStructs.msg_text vkMsg of
  Nothing -> Nothing
  Just "" -> Nothing
  _ -> pure $ PureStructs.MsgTypeCommon "Message"
