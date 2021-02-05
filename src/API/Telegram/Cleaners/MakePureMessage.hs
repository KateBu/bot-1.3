module API.Telegram.Cleaners.MakePureMessage where

import API.Telegram.Cleaners.MbMsgType
  ( mbAnimation,
    mbAudio,
    mbContact,
    mbDocument,
    mbLocation,
    mbPhoto,
    mbPoll,
    mbSticker,
    mbTextMessage,
    mbVenue,
    mbVideo,
    mbVoice,
  )
import qualified API.Telegram.Structs.Updates as TStructs
import Control.Applicative (Alternative ((<|>)))
import Data.Maybe (fromMaybe)
import qualified Environment.Exports as Env
import qualified Exceptions.Exports as BotEx
import qualified Logic.PureStructs as PureStructs
import qualified TextMessages.LoggerMessages as LoggerMsgs

telUpdateToPureMessage ::
  Env.HelpMessage ->
  TStructs.TelUpdateResult ->
  PureStructs.PureMessage
telUpdateToPureMessage helpMsg telUpdateResult = do
  let updateId = TStructs.update_id telUpdateResult
  let mbPureMessage =
        mbMakeCallbackPureMessage telUpdateResult updateId
          <|> mbMakePureMessage helpMsg telUpdateResult updateId
  fromMaybe (BotEx.throwUpdateExceptUnwrapped LoggerMsgs.noUpdates) mbPureMessage

mbMakeCallbackPureMessage ::
  TStructs.TelUpdateResult ->
  PureStructs.UpdateID ->
  Maybe PureStructs.PureMessage
mbMakeCallbackPureMessage telUpdateResult updateId =
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

mbMakePureMessage ::
  Env.HelpMessage ->
  TStructs.TelUpdateResult ->
  PureStructs.UpdateID ->
  Maybe PureStructs.PureMessage
mbMakePureMessage helpMsg telUpdateResult updateId =
  case TStructs.message_info telUpdateResult of
    Nothing -> pure $ PureStructs.PureMessage PureStructs.MsgTypeEmpty updateId Nothing Nothing
    Just msgInfo -> do
      let chatId = TStructs.chat_id $ TStructs.chat msgInfo
      makeCommonMessage helpMsg updateId chatId msgInfo

makeCommonMessage ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
makeCommonMessage helpMsg updateId chatId msgInfo =
  mbAnimation updateId chatId msgInfo
    <|> mbAudio updateId chatId msgInfo
    <|> mbDocument updateId chatId msgInfo
    <|> mbVideo updateId chatId msgInfo
    <|> mbVoice updateId chatId msgInfo
    <|> mbPhoto updateId chatId msgInfo
    <|> mbContact updateId chatId msgInfo
    <|> mbVenue updateId chatId msgInfo
    <|> mbLocation updateId chatId msgInfo
    <|> mbSticker updateId chatId msgInfo
    <|> mbPoll updateId chatId msgInfo
    <|> mbTextMessage helpMsg updateId chatId msgInfo
