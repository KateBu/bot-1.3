module API.Telegram.Cleaners.Attachments.MbVoice where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbVoice ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbVoice updateId chatId msgInfo = do
  voiceInfo <- TStructs.voice msgInfo
  mbVoice' updateId chatId msgInfo voiceInfo

mbVoice' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelVoice ->
  Maybe PureStructs.PureMessage
mbVoice' updateId chatId msgInfo voiceInfo =
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
