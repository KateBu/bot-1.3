module API.VK.Functions.MessageTypes.Attachment where

import qualified API.PureStructs.Exports as PureStructs
import qualified API.VK.Structs.Exports as VK

buildAttachmentMessage :: VK.Message -> Maybe PureStructs.MessageType
buildAttachmentMessage msg = VK.attachments msg >>= mbAttachmentMsg'
  where
    mbAttachmentMsg' [] = Nothing
    mbAttachmentMsg' _ = pure $ PureStructs.MsgTypeCommon "Attachment"
