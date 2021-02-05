module API.Telegram.Cleaners.Attachments.MbPhoto where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbPhoto ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbPhoto updateId chatId msgInfo = do
  photoInfo <- TStructs.photo msgInfo
  mbPhoto' updateId chatId msgInfo photoInfo

mbPhoto' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  [TStructs.TelPhoto] ->
  Maybe PureStructs.PureMessage
mbPhoto' updateId chatId msgInfo photoInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Photo")
      updateId
      (Just chatId)
      photoParams
  where
    photoParams =
      Just $
        basicParams chatId msgInfo
          <> getPhotoParams photoInfo

getPhotoParams :: [TStructs.TelPhoto] -> [PureStructs.Params]
getPhotoParams [] = []
getPhotoParams (x : _) = [PureStructs.ParamsText "photo" (TStructs.photo_file_id x)]
