module API.VK.Cleaners.MsgTypes.Geo where

import qualified Logic.PureStructs as PureStructs
import qualified API.VK.Structs as VKStructs 

mbGeo :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
mbGeo vkMsg = case VKStructs.geo vkMsg of
  Nothing -> Nothing
  _ -> pure $ PureStructs.MTCommon "Geo"
