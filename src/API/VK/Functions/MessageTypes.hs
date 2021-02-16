module API.VK.Functions.MessageTypes where

import qualified API.PureStructs.Exports as PureStructs
import API.VK.Functions.MessageTypes.Attachment (buildAttachmentMessage)
import API.VK.Functions.MessageTypes.Callback (buildCallbackMessage)
import API.VK.Functions.MessageTypes.Forward (buildFwdMessage)
import API.VK.Functions.MessageTypes.Geo (buildGeoMessage)
import API.VK.Functions.MessageTypes.Text (buildTextMessage)
import API.VK.Functions.MessageTypes.UserCommand (buildCommandMessage)
import qualified API.VK.Structs.Exports as VK
import Control.Applicative (Alternative ((<|>)))

buildMessageType :: VK.Message -> Maybe PureStructs.MessageType
buildMessageType msg =
  buildCallbackMessage msg
    <|> buildCommandMessage msg
    <|> buildAttachmentMessage msg
    <|> buildGeoMessage msg
    <|> buildFwdMessage msg
    <|> buildTextMessage msg
