module API.Telegram.Functions.Attachments.Sticker (buildStickerMessage) where

import API.Telegram.Functions.Params (basicParams)
import qualified API.Telegram.Structs.MessageInfo as Telegram
import qualified Logic.Structs as PureStructs

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
        basicParams chatId msgInfo
          <> [ PureStructs.ParamsText "sticker" (Telegram.sticker_file_id stickerInfo)
             ]
