module API.Telegram.Functions.Attachments.Photo (buildPhotoMessage) where

import qualified API.PureStructs.Exports as PureStructs
import API.Telegram.Functions.Params (buildBasicParams)
import qualified API.Telegram.Structs.MessageInfo as Telegram

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
        buildBasicParams chatId msgInfo
          <> buildPhotoParams photoInfo

buildPhotoParams :: [Telegram.Photo] -> [PureStructs.Params]
buildPhotoParams [] = []
buildPhotoParams (x : _) = [PureStructs.ParamsText "photo" (Telegram.photo_file_id x)]
