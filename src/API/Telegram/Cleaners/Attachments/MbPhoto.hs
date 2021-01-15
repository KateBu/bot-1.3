module API.Telegram.Cleaners.Attachments.MbPhoto where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.TStructs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbPhoto ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbPhoto uid chid mInfo =
  TStructs.photo mInfo >>= mbPhoto' uid chid mInfo

mbPhoto' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  [TStructs.TelPhoto] ->
  Maybe PureStructs.PureMessage
mbPhoto' uid chid mInfo photo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MTCommon "Photo")
      uid
      (Just chid)
      ( Just $
          basicParams chid mInfo
            <> getPhotoParams photo
      )

getPhotoParams :: [TStructs.TelPhoto] -> [PureStructs.Params]
getPhotoParams [] = []
getPhotoParams (x : _) = [PureStructs.ParamsText "photo" (TStructs.photo_file_id x)]
