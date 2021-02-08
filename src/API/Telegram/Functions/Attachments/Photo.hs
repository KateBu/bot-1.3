module API.Telegram.Functions.Attachments.Photo where

import API.Telegram.Functions.BasicParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.Structs as PureStructs

buildPhotoMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
buildPhotoMessage updateId chatId msgInfo = do
  photoInfo <- TStructs.photo msgInfo
  buildPhotoMessage' updateId chatId msgInfo photoInfo

buildPhotoMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  [TStructs.TelPhoto] ->
  Maybe PureStructs.PureMessage
buildPhotoMessage' updateId chatId msgInfo photoInfo =
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
          <> buildPhotoParams photoInfo

buildPhotoParams :: [TStructs.TelPhoto] -> [PureStructs.Params]
buildPhotoParams [] = []
buildPhotoParams (x : _) = [PureStructs.ParamsText "photo" (TStructs.photo_file_id x)]
