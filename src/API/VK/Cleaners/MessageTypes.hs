module API.VK.Cleaners.MessageTypes where

import qualified API.VK.Structs as VKStructs
import Control.Applicative (Alternative ((<|>)))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs

mbCallBackMsg,
  mbUserCommand,
  mbAttachmentMsg,
  mbTextMsg,
  mbGeo,
  mbFwd ::
    VKStructs.VKMessage -> Maybe PureStructs.MessageType
mbCallBackMsg vkMsg =
  PureStructs.MTCallbackQuery <$> mbNewRep (fromMaybe "" $ VKStructs.cbPayload vkMsg)

mbNewRep :: T.Text -> Maybe T.Text
mbNewRep callback =
  mbRep1 callback
    <|> mbRep2 callback
    <|> mbRep3 callback
    <|> mbRep4 callback
    <|> mbRep5 callback

mbUserCommand vkMsg = maybe Nothing mbUserCommand' $ VKStructs.msgText vkMsg

mbUserCommand' :: T.Text -> Maybe PureStructs.MessageType
mbUserCommand' "/help" = pure $ PureStructs.MTUserCommand PureStructs.Help
mbUserCommand' "/repeat" = pure $ PureStructs.MTUserCommand PureStructs.Repeat
mbUserCommand' _ = Nothing

mbAttachmentMsg vkMsg = maybe Nothing mbAttachmentMsg' $ VKStructs.attachments vkMsg
  where
    mbAttachmentMsg' [] = Nothing
    mbAttachmentMsg' _ = pure $ PureStructs.MTCommon "Attachment"

mbTextMsg vkMsg = case VKStructs.msgText vkMsg of
  Nothing -> Nothing
  Just "" -> Nothing
  _ -> pure $ PureStructs.MTCommon "Message"

mbGeo vkMsg = case VKStructs.geo vkMsg of
  Nothing -> Nothing
  _ -> pure $ PureStructs.MTCommon "Geo"

mbFwd vkMsg = case VKStructs.fwdMessages vkMsg of
  Nothing -> Nothing
  _ -> pure $ PureStructs.MTCommon "Fwd"

mbRep1 :: T.Text -> Maybe T.Text
mbRep1 txt =
  if T.isInfixOf PureStructs.rep1 txt
    then pure PureStructs.rep1
    else Nothing

mbRep2 :: T.Text -> Maybe T.Text
mbRep2 txt =
  if T.isInfixOf PureStructs.rep2 txt
    then pure PureStructs.rep2
    else Nothing

mbRep3 :: T.Text -> Maybe T.Text
mbRep3 txt =
  if T.isInfixOf PureStructs.rep3 txt
    then pure PureStructs.rep3
    else Nothing

mbRep4 :: T.Text -> Maybe T.Text
mbRep4 txt =
  if T.isInfixOf PureStructs.rep4 txt
    then pure PureStructs.rep4
    else Nothing

mbRep5 :: T.Text -> Maybe T.Text
mbRep5 txt =
  if T.isInfixOf PureStructs.rep5 txt
    then pure PureStructs.rep5
    else Nothing

getMessageType :: VKStructs.VKMessage -> Either BotEx.BotException PureStructs.MessageType
getMessageType vkMsg = maybe (BotEx.throwOtherException LoggerMsgs.notImplemented) Right msgType
  where
    msgType =
      mbCallBackMsg vkMsg
        <|> mbUserCommand vkMsg
        <|> mbAttachmentMsg vkMsg
        <|> mbGeo vkMsg
        <|> mbFwd vkMsg
        <|> mbTextMsg vkMsg
