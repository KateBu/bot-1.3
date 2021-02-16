module API.VK.Functions.MessageTypes.Forward where

import qualified API.PureStructs.Exports as PureStructs
import qualified API.VK.Structs.Exports as VK

buildFwdMessage :: VK.Message -> Maybe PureStructs.MessageType
buildFwdMessage msg = case VK.fwd_msgs msg of
  Nothing -> Nothing
  _ -> pure $ PureStructs.MsgTypeCommon "Fwd"
