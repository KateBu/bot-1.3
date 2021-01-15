module API.Telegram.Cleaners.Attachments.MbVideo where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.TStructs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbVideo ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbVideo uid chid mInfo =
  TStructs.video mInfo >>= mbVideo' uid chid mInfo

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
      ( Just $
          basicParams chid mInfo
            <> [PureStructs.ParamsText "video" (TStructs.video_file_id video)]
      )
