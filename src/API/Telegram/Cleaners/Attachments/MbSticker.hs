module API.Telegram.Cleaners.Attachments.MbSticker where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.TStructs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbSticker ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbSticker uid chid mInfo =
  let mbSt = TStructs.sticker mInfo
   in maybe Nothing (mbSticker' uid chid mInfo) mbSt

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
      ( Just $
          basicParams chid mInfo
            <> [ PureStructs.ParamsText "sticker" (TStructs.s_file_id sticker)
               ]
      )
