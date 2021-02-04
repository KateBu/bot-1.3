module API.Telegram.Cleaners.Attachments.MbVoice where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbVoice ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbVoice uid chid mInfo = do
  voiceInfo <- TStructs.voice mInfo
  mbVoice' uid chid mInfo voiceInfo

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
      voiceParams
  where
    voiceParams =
      Just $
        basicParams chid mInfo
          <> [PureStructs.ParamsText "voice" (TStructs.voice_file_id voice)]
