module API.Telegram.Cleaners.Attachments.MbSticker where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbSticker ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbSticker uid chid mInfo =
  TStructs.sticker mInfo >>= mbSticker' uid chid mInfo

mbSticker' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelSticker ->
  Maybe PureStructs.PureMessage
mbSticker' uid chid mInfo sticker =
  pure $
    PureStructs.PureMessage
      (PureStructs.MTCommon "Sticker")
      uid
      (Just chid)
      stickerParams
  where
    stickerParams =
      Just $
        basicParams chid mInfo
          <> [ PureStructs.ParamsText "sticker" (TStructs.s_file_id sticker)
             ]
