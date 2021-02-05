module API.Telegram.Cleaners.Attachments.MbAudio where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbAudio ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbAudio updatId chatId msgInfo = do
  audioInfo <- TStructs.audio msgInfo
  mbAudio' updatId chatId msgInfo audioInfo

mbAudio' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelAudio ->
  Maybe PureStructs.PureMessage
mbAudio' updatId chatId msgInfo audioInfo =
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
