module API.VK.Functions.MsgTypes.Attachment where

import qualified API.VK.Structs.Exports as VK
import qualified Logic.Structs as PureStructs

buildAttachmentMessage :: VK.Message -> Maybe PureStructs.MessageType
buildAttachmentMessage msg = VK.attachments msg >>= mbAttachmentMsg'
  where
    mbAttachmentMsg' [] = Nothing
    mbAttachmentMsg' _ = pure $ PureStructs.MsgTypeCommon "Attachment"
