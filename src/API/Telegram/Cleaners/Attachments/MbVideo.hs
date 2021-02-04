module API.Telegram.Cleaners.Attachments.MbVideo where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbVideo ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbVideo uid chid mInfo = do
  videoInfo <- TStructs.video mInfo
  mbVideo' uid chid mInfo videoInfo

mbVideo' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelVideo ->
  Maybe PureStructs.PureMessage
mbVideo' uid chid mInfo video =
  pure $
    PureStructs.PureMessage
      (PureStructs.MTCommon "Video")
      uid
      (Just chid)
      videoParams
  where
    videoParams =
      Just $
        basicParams chid mInfo
          <> [PureStructs.ParamsText "video" (TStructs.video_file_id video)]
