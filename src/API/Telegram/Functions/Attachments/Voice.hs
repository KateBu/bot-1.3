module API.Telegram.Functions.Attachments.Voice where

import API.Telegram.Functions.BasicParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.Structs as PureStructs

buildVoiceMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
buildVoiceMessage updateId chatId msgInfo = do
  voiceInfo <- TStructs.voice msgInfo
  buildVoiceMessage' updateId chatId msgInfo voiceInfo

buildVoiceMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelVoice ->
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
        basicParams chatId msgInfo
          <> [PureStructs.ParamsText "voice" (TStructs.voice_file_id voiceInfo)]
