module API.VK.Functions.MessageTypes.Geo where

import qualified API.PureStructs.Exports as PureStructs
import qualified API.VK.Structs.Exports as VK

buildGeoMessage :: VK.Message -> Maybe PureStructs.MessageType
buildGeoMessage msg = case VK.geo msg of
  Nothing -> Nothing
  _ -> pure $ PureStructs.MsgTypeCommon "Geo"
