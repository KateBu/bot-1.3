module API.Telegram.Cleaners.Attachments.MbSticker where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbSticker ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbSticker updateId chatId msgInfo = do
  stickerInfo <- TStructs.sticker msgInfo
  mbSticker' updateId chatId msgInfo stickerInfo

mbSticker' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelSticker ->
  Maybe PureStructs.PureMessage
mbSticker' updateId chatId msgInfo stickerInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Sticker")
      updateId
      (Just chatId)
      stickerParams
  where
    stickerParams =
      Just $
        basicParams chatId msgInfo
          <> [ PureStructs.ParamsText "sticker" (TStructs.sticker_file_id stickerInfo)
             ]
