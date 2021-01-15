module API.VK.Cleaners.MsgTypes.Fwd where

import qualified Logic.PureStructs as PureStructs
import qualified API.VK.Structs as VKStructs 

mbFwd :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
mbFwd vkMsg = case VKStructs.fwdMessages vkMsg of
  Nothing -> Nothing
  _ -> pure $ PureStructs.MTCommon "Fwd"