module API.Telegram.Cleaners.Attachments.MbVoice where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.TStructs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbVoice ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbVoice uid chid mInfo =
  TStructs.voice mInfo >>= mbVoice' uid chid mInfo

mbVoice' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelVoice ->
  Maybe PureStructs.PureMessage
mbVoice' uid chid mInfo voice =
  pure $
    PureStructs.PureMessage
      (PureStructs.MTCommon "Voice")
      uid
      (Just chid)
      ( Just $
          basicParams chid mInfo
            <> [PureStructs.ParamsText "voice" (TStructs.voice_file_id voice)]
      )
