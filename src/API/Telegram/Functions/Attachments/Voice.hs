module API.Telegram.Functions.Attachments.Voice (buildVoiceMessage) where

import qualified API.PureStructs.Exports as PureStructs
import API.Telegram.Functions.Params (buildBasicParams)
import qualified API.Telegram.Structs.MessageInfo as Telegram

buildVoiceMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Maybe PureStructs.PureMessage
buildVoiceMessage updateId chatId msgInfo = do
  voiceInfo <- Telegram.voice msgInfo
  buildVoiceMessage' updateId chatId msgInfo voiceInfo

buildVoiceMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Telegram.Voice ->
  Maybe PureStructs.PureMessage
buildVoiceMessage' updateId chatId msgInfo voiceInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Voice")
      updateId
      (Just chatId)
      voiceParams
  where
    voiceParams =
      Just $
        buildBasicParams chatId msgInfo
          <> [PureStructs.ParamsText "voice" (Telegram.voice_file_id voiceInfo)]
