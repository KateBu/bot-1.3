module API.VK.Cleaners.MessageTypes where

import API.VK.Cleaners.MsgTypes.Attachment (mbAttachmentMsg)
import API.VK.Cleaners.MsgTypes.Callback (mbCallBackMsg)
import API.VK.Cleaners.MsgTypes.Fwd (mbFwd)
import API.VK.Cleaners.MsgTypes.Geo (mbGeo)
import API.VK.Cleaners.MsgTypes.TextMsg (mbTextMsg)
import API.VK.Cleaners.MsgTypes.UserCommand (mbUserCommand)
import qualified API.VK.Structs as VKStructs
import Control.Applicative (Alternative ((<|>)))
import Data.Maybe (fromMaybe)
import qualified Environment.Logger.LoggerMsgs as LoggerMsgs
import qualified Exceptions.Exceptions as BotEx
import qualified Logic.PureStructs as PureStructs

getMessageType :: VKStructs.VKMessage -> PureStructs.MessageType
getMessageType vkMsg = fromMaybe (BotEx.throwPureOtherException LoggerMsgs.notImplemented) msgType
  where
    msgType =
      mbCallBackMsg vkMsg
        <|> mbUserCommand vkMsg
        <|> mbAttachmentMsg vkMsg
        <|> mbGeo vkMsg
        <|> mbFwd vkMsg
        <|> mbTextMsg vkMsg
