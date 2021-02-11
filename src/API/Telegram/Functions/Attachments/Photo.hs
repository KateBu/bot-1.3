module API.Telegram.Functions.Attachments.Photo (buildPhotoMessage) where

import API.Telegram.Functions.Params (basicParams)
import qualified API.Telegram.Structs.MessageInfo as Telegram
import qualified Logic.Structs as PureStructs

buildPhotoMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Maybe PureStructs.PureMessage
buildPhotoMessage updateId chatId msgInfo = do
  photoInfo <- Telegram.photo msgInfo
  buildPhotoMessage' updateId chatId msgInfo photoInfo

buildPhotoMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  [Telegram.Photo] ->
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

buildPhotoParams :: [Telegram.Photo] -> [PureStructs.Params]
buildPhotoParams [] = []
buildPhotoParams (x : _) = [PureStructs.ParamsText "photo" (Telegram.photo_file_id x)]
