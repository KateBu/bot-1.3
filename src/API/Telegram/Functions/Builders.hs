module API.Telegram.Functions.Builders where

import API.Telegram.Functions.Attachments
  ( buildAnimationMessage,
    buildAudioMessage,
    buildContactMessage,
    buildDocumentMessage,
    buildLocationMessage,
    buildPhotoMessage,
    buildPollMessage,
    buildStickerMessage,
    buildTextMessage,
    buildVenueMessage,
    buildVideoMessage,
    buildVoiceMessage,
  )
import qualified API.Telegram.Structs.Updates as TStructs
import Control.Applicative (Alternative ((<|>)))
import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs

buildPureMessage ::
  Env.HelpMessage ->
  TStructs.TelUpdateResult ->
  Maybe PureStructs.PureMessage
buildPureMessage helpMsg telUpdateResult =
  buildCallbackMessage telUpdateResult updateId
    <|> buildPureMessage' helpMsg telUpdateResult updateId
  where
    updateId = TStructs.update_id telUpdateResult

buildPureMessage' ::
  Env.HelpMessage ->
  TStructs.TelUpdateResult ->
  PureStructs.UpdateID ->
  Maybe PureStructs.PureMessage
buildPureMessage' helpMsg telUpdateResult updateId =
  case TStructs.message_info telUpdateResult of
    Nothing -> pure $ PureStructs.PureMessage PureStructs.MsgTypeEmpty updateId Nothing Nothing
    Just msgInfo -> do
      let chatId = TStructs.chat_id $ TStructs.chat msgInfo
      buildCommonMessage helpMsg updateId chatId msgInfo

buildCallbackMessage ::
  TStructs.TelUpdateResult ->
  PureStructs.UpdateID ->
  Maybe PureStructs.PureMessage
buildCallbackMessage telUpdateResult updateId =
  case TStructs.callback_query telUpdateResult of
    Just (TStructs.Callback callbackMsg callbackData) -> do
      let chatId = (TStructs.callback_chat_id . TStructs.callback_chat) callbackMsg
      pure
        ( PureStructs.PureMessage
            (PureStructs.MsgTypeCallbackQuery callbackData)
            updateId
            (Just chatId)
            (callbackParams chatId)
        )
      where
        callbackParams chatId =
          Just
            [ PureStructs.ParamsText "text" $ PureStructs.newRepeatText (PureStructs.getNewRepeatNumber callbackData),
              PureStructs.ParamsNum "chat_id" chatId
            ]
    _ -> Nothing

buildCommonMessage ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
buildCommonMessage helpMsg updateId chatId msgInfo =
  buildAnimationMessage updateId chatId msgInfo
    <|> buildAudioMessage updateId chatId msgInfo
    <|> buildDocumentMessage updateId chatId msgInfo
    <|> buildVideoMessage updateId chatId msgInfo
    <|> buildVoiceMessage updateId chatId msgInfo
    <|> buildPhotoMessage updateId chatId msgInfo
    <|> buildContactMessage updateId chatId msgInfo
    <|> buildVenueMessage updateId chatId msgInfo
    <|> buildLocationMessage updateId chatId msgInfo
    <|> buildStickerMessage updateId chatId msgInfo
    <|> buildPollMessage updateId chatId msgInfo
    <|> buildTextMessage helpMsg updateId chatId msgInfo
