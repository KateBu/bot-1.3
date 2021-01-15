module API.VK.Cleaners.MsgTypes.Attachment where

import qualified Logic.PureStructs as PureStructs
import qualified API.VK.Structs as VKStructs 

mbAttachmentMsg :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
mbAttachmentMsg vkMsg = VKStructs.attachments vkMsg >>= mbAttachmentMsg'
  where
    mbAttachmentMsg' [] = Nothing
    mbAttachmentMsg' _ = pure $ PureStructs.MTCommon "Attachment"