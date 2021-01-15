module API.VK.Cleaners.MessageTypes where

import qualified API.VK.Structs as VKStructs
import Control.Applicative (Alternative ((<|>)))
import Data.Maybe (fromMaybe)
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs
import API.VK.Cleaners.MsgTypes.Callback ( mbCallBackMsg ) 
import API.VK.Cleaners.MsgTypes.UserCommand ( mbUserCommand ) 
import API.VK.Cleaners.MsgTypes.Attachment ( mbAttachmentMsg )
import API.VK.Cleaners.MsgTypes.TextMsg ( mbTextMsg ) 
import API.VK.Cleaners.MsgTypes.Geo ( mbGeo ) 
import API.VK.Cleaners.MsgTypes.Fwd ( mbFwd )

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
