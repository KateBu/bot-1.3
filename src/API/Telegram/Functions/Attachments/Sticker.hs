module API.Telegram.Functions.Attachments.Sticker where

import API.Telegram.Functions.Params (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.Structs as PureStructs

buildStickerMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
buildStickerMessage updateId chatId msgInfo = do
  stickerInfo <- TStructs.sticker msgInfo
  buildStickerMessage' updateId chatId msgInfo stickerInfo

buildStickerMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelSticker ->
  Maybe PureStructs.PureMessage
buildStickerMessage' updateId chatId msgInfo stickerInfo =
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
