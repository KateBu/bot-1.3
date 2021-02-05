module API.Telegram.Cleaners.Attachments.MbVideo where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbVideo ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbVideo updateId chatId msgInfo = do
  videoInfo <- TStructs.video msgInfo
  mbVideo' updateId chatId msgInfo videoInfo

mbVideo' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelVideo ->
  Maybe PureStructs.PureMessage
mbVideo' updateId chatId msgInfo videoInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Video")
      updateId
      (Just chatId)
      videoParams
  where
    videoParams =
      Just $
        basicParams chatId msgInfo
          <> [PureStructs.ParamsText "video" (TStructs.video_file_id videoInfo)]
