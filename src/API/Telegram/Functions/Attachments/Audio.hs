module API.Telegram.Functions.Attachments.Audio (buildAudioMessage) where

import qualified API.PureStructs.Exports as PureStructs
import API.Telegram.Functions.Params (buildBasicParams)
import qualified API.Telegram.Structs.MessageInfo as Telegram

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
        buildBasicParams chatId msgInfo
          <> [PureStructs.ParamsText "audio" (Telegram.audio_id audioInfo)]
