module API.VK.Cleaners.MsgTypes.Geo where

import qualified API.VK.Structs.Exports as VKStructs
import qualified Logic.PureStructs as PureStructs

mbGeo :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
mbGeo vkMsg = case VKStructs.geo vkMsg of
  Nothing -> Nothing
  _ -> pure $ PureStructs.MTCommon "Geo"
