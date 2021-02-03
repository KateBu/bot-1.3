module API.VK.Cleaners.MsgTypes.Fwd where

import qualified API.VK.Structs.Exports as VKStructs
import qualified Logic.PureStructs as PureStructs

mbFwd :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
mbFwd vkMsg = case VKStructs.fwdMessages vkMsg of
  Nothing -> Nothing
  _ -> pure $ PureStructs.MTCommon "Fwd"
