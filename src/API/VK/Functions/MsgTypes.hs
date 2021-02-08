module API.VK.Functions.MsgTypes where

import API.VK.Functions.MsgTypes.Attachment (buildAttachmentMessage)
import API.VK.Functions.MsgTypes.Callback (buildCallbackMessage)
import API.VK.Functions.MsgTypes.Fwd (buildFwdMessage)
import API.VK.Functions.MsgTypes.Geo (buildGeoMessage)
import API.VK.Functions.MsgTypes.TextMsg (buildTextMessage)
import API.VK.Functions.MsgTypes.UserCommand (buildCommandMessage)
import qualified API.VK.Structs.Exports as VKStructs
import Control.Applicative (Alternative ((<|>)))
import qualified Logic.Structs as PureStructs

getMessageType :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
getMessageType vkMsg =
  buildCallbackMessage vkMsg
    <|> buildCommandMessage vkMsg
    <|> buildAttachmentMessage vkMsg
    <|> buildGeoMessage vkMsg
    <|> buildFwdMessage vkMsg
    <|> buildTextMessage vkMsg
