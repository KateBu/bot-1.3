module API.VK.Functions.MsgTypes.Fwd where

import qualified API.VK.Structs.Exports as VK
import qualified Logic.Structs as PureStructs

buildFwdMessage :: VK.Message -> Maybe PureStructs.MessageType
buildFwdMessage msg = case VK.fwd_msgs msg of
  Nothing -> Nothing
  _ -> pure $ PureStructs.MsgTypeCommon "Fwd"
