module API.Telegram.Cleaners.Attachments.MbAudio where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.TStructs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbAudio ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbAudio uid chid mInfo =
  TStructs.audio mInfo >>= mbAudio' uid chid mInfo

mbAudio' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelAudio ->
  Maybe PureStructs.PureMessage
mbAudio' uid chid mInfo aud =
  pure $
    PureStructs.PureMessage
      (PureStructs.MTCommon "Audio")
      uid
      (Just chid)
      ( Just $
          basicParams chid mInfo
            <> [PureStructs.ParamsText "audio" (TStructs.audio_id aud)]
      )
