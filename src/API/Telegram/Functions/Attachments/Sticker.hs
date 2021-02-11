module API.Telegram.Functions.Attachments.Sticker (buildStickerMessage) where

import qualified API.PureStructs.Exports as PureStructs
import API.Telegram.Functions.Params (buildBasicParams)
import qualified API.Telegram.Structs.MessageInfo as Telegram

buildStickerMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Maybe PureStructs.PureMessage
buildStickerMessage updateId chatId msgInfo = do
  stickerInfo <- Telegram.sticker msgInfo
  buildStickerMessage' updateId chatId msgInfo stickerInfo

buildStickerMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Telegram.Sticker ->
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
        buildBasicParams chatId msgInfo
          <> [ PureStructs.ParamsText "sticker" (Telegram.sticker_file_id stickerInfo)
             ]
