module API.VK.Cleaners.GetMessageType where

import API.VK.Cleaners.MsgTypes.Attachment (mbAttachmentMsg)
import API.VK.Cleaners.MsgTypes.Callback (mbCallbackMsg)
import API.VK.Cleaners.MsgTypes.Fwd (mbFwd)
import API.VK.Cleaners.MsgTypes.Geo (mbGeo)
import API.VK.Cleaners.MsgTypes.TextMsg (mbTextMsg)
import API.VK.Cleaners.MsgTypes.UserCommand (mbUserCommand)
import qualified API.VK.Structs.Exports as VKStructs
import Control.Applicative (Alternative ((<|>)))
import qualified Logic.PureStructs as PureStructs

getMessageType :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
getMessageType vkMsg =
  mbCallbackMsg vkMsg
    <|> mbUserCommand vkMsg
    <|> mbAttachmentMsg vkMsg
    <|> mbGeo vkMsg
    <|> mbFwd vkMsg
    <|> mbTextMsg vkMsg
