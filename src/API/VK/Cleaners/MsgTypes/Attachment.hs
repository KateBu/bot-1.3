module API.VK.Cleaners.MsgTypes.Attachment where

import qualified API.VK.Structs.Internals as VKStructs
import qualified Logic.PureStructs as PureStructs

mbAttachmentMsg :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
mbAttachmentMsg vkMsg = VKStructs.attachments vkMsg >>= mbAttachmentMsg'
  where
    mbAttachmentMsg' [] = Nothing
    mbAttachmentMsg' _ = pure $ PureStructs.MTCommon "Attachment"
