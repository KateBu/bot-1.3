module API.VK.Functions.MsgTypes.Geo where

import qualified API.VK.Structs.Exports as VK
import qualified Logic.Structs as PureStructs

buildGeoMessage :: VK.Message -> Maybe PureStructs.MessageType
buildGeoMessage msg = case VK.geo msg of
  Nothing -> Nothing
  _ -> pure $ PureStructs.MsgTypeCommon "Geo"
