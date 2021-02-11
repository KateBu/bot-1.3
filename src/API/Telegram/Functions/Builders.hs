module API.Telegram.Functions.Builders (buildPureMessage) where

import qualified API.PureStructs.Exports as PureStructs
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
import qualified API.Telegram.Structs.Updates as Telegram
import Control.Applicative (Alternative ((<|>)))
import qualified Environment.Exports as Env
import qualified TextMessages.RepeatCommandMessages as RepeatCommandMessages

buildPureMessage ::
  Env.HelpMessage ->
  Telegram.UpdateResult ->
  Maybe PureStructs.PureMessage
buildPureMessage helpMsg updateResult =
  buildCallbackMessage updateResult updateId
    <|> buildPureMessage' helpMsg updateResult updateId
  where
    updateId = Telegram.update_id updateResult

buildPureMessage' ::
  Env.HelpMessage ->
  Telegram.UpdateResult ->
  PureStructs.UpdateID ->
  Maybe PureStructs.PureMessage
buildPureMessage' helpMsg updateResult updateId =
  case Telegram.message_info updateResult of
    Nothing -> pure $ PureStructs.PureMessage PureStructs.MsgTypeEmpty updateId Nothing Nothing
    Just msgInfo -> do
      let chatId = Telegram.chat_id $ Telegram.chat msgInfo
      buildCommonMessage helpMsg updateId chatId msgInfo

buildCallbackMessage ::
  Telegram.UpdateResult ->
  PureStructs.UpdateID ->
  Maybe PureStructs.PureMessage
buildCallbackMessage updateResult updateId =
  case Telegram.callback_query updateResult of
    Just (Telegram.Callback callbackMsg callbackData) -> do
      let chatId = (Telegram.callback_chat_id . Telegram.callback_chat) callbackMsg
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
            [ PureStructs.ParamsText "text" $
                RepeatCommandMessages.newRepeatText
                  (PureStructs.getNewRepeatNumber callbackData),
              PureStructs.ParamsNum "chat_id" chatId
            ]
    _ -> Nothing

buildCommonMessage ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
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
