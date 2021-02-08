module API.VK.Functions.MsgTypes.Attachment where

import qualified API.VK.Structs.Exports as VKStructs
import qualified Logic.PureStructs as PureStructs

buildAttachmentMessage :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
buildAttachmentMessage vkMsg = VKStructs.attachments vkMsg >>= mbAttachmentMsg'
  where
    mbAttachmentMsg' [] = Nothing
    mbAttachmentMsg' _ = pure $ PureStructs.MsgTypeCommon "Attachment"
