module API.Telegram.Functions.Attachments.Audio where

import API.Telegram.Functions.BasicParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

buildAudioMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
buildAudioMessage updatId chatId msgInfo = do
  audioInfo <- TStructs.audio msgInfo
  buildAudioMessage' updatId chatId msgInfo audioInfo

buildAudioMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelAudio ->
  Maybe PureStructs.PureMessage
buildAudioMessage' updatId chatId msgInfo audioInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Audio")
      updatId
      (Just chatId)
      audioParams
  where
    audioParams =
      Just $
        basicParams chatId msgInfo
          <> [PureStructs.ParamsText "audio" (TStructs.audio_id audioInfo)]
