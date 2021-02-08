module API.VK.Functions.MsgTypes.Fwd where

import qualified API.VK.Structs.Exports as VKStructs
import qualified Logic.PureStructs as PureStructs

buildFwdMessage :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
buildFwdMessage vkMsg = case VKStructs.fwd_msgs vkMsg of
  Nothing -> Nothing
  _ -> pure $ PureStructs.MsgTypeCommon "Fwd"
