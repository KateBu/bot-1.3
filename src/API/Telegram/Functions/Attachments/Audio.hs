module API.Telegram.Functions.Attachments.Audio (buildAudioMessage) where

import API.Telegram.Functions.Params (basicParams)
import qualified API.Telegram.Structs.MessageInfo as Telegram
import qualified Logic.Structs as PureStructs

buildAudioMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Maybe PureStructs.PureMessage
buildAudioMessage updatId chatId msgInfo = do
  audioInfo <- Telegram.audio msgInfo
  buildAudioMessage' updatId chatId msgInfo audioInfo

buildAudioMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Telegram.Audio ->
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
          <> [PureStructs.ParamsText "audio" (Telegram.audio_id audioInfo)]
