module API.VK.Functions.MsgTypes.Geo where

import qualified API.VK.Structs.Exports as VKStructs
import qualified Logic.Structs as PureStructs

buildGeoMessage :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
buildGeoMessage vkMsg = case VKStructs.geo vkMsg of
  Nothing -> Nothing
  _ -> pure $ PureStructs.MsgTypeCommon "Geo"
