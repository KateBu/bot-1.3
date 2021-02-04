module API.Telegram.Cleaners.Attachments.MbAudio where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbAudio ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbAudio uid chid mInfo = do 
  audioInfo <- TStructs.audio mInfo
  mbAudio' uid chid mInfo audioInfo 

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
      audioParams
  where
    audioParams =
      Just $
        basicParams chid mInfo
          <> [PureStructs.ParamsText "audio" (TStructs.audio_id aud)]
