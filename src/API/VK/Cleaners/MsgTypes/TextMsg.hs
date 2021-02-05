module API.VK.Cleaners.MsgTypes.TextMsg where

import qualified API.VK.Structs.Exports as VKStructs
import qualified Logic.PureStructs as PureStructs

mbTextMsg :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
mbTextMsg vkMsg = case VKStructs.msg_text vkMsg of
  Nothing -> Nothing
  Just "" -> Nothing
  _ -> pure $ PureStructs.MsgTypeCommon "Message"
