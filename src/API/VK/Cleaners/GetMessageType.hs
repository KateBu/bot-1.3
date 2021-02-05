module API.VK.Cleaners.GetMessageType where

import API.VK.Cleaners.MsgTypes.Attachment (mbAttachmentMsg)
import API.VK.Cleaners.MsgTypes.Callback (mbCallbackMsg)
import API.VK.Cleaners.MsgTypes.Fwd (mbFwd)
import API.VK.Cleaners.MsgTypes.Geo (mbGeo)
import API.VK.Cleaners.MsgTypes.TextMsg (mbTextMsg)
import API.VK.Cleaners.MsgTypes.UserCommand (mbUserCommand)
import qualified API.VK.Structs.Exports as VKStructs
import Control.Applicative (Alternative ((<|>)))
import Data.Maybe (fromMaybe)
import qualified Exceptions.Exports as BotEx
import qualified Logic.PureStructs as PureStructs
import qualified TextMessages.LoggerMessages as LoggerMsgs

getMessageType :: VKStructs.VKMessage -> PureStructs.MessageType
getMessageType vkMsg = fromMaybe (BotEx.throwOtherExceptionUnwrapped errMsg) msgType
  where
    errMsg = LoggerMsgs.vkMsgTypeNotImplemented
    msgType =
      mbCallbackMsg vkMsg
        <|> mbUserCommand vkMsg
        <|> mbAttachmentMsg vkMsg
        <|> mbGeo vkMsg
        <|> mbFwd vkMsg
        <|> mbTextMsg vkMsg
