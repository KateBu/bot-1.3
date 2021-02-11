module API.Telegram.Functions.Attachments.Video (buildVideoMessage) where

import API.Telegram.Functions.Params (basicParams)
import qualified API.Telegram.Structs.MessageInfo as Telegram
import qualified Logic.Structs as PureStructs

buildVideoMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Maybe PureStructs.PureMessage
buildVideoMessage updateId chatId msgInfo = do
  videoInfo <- Telegram.video msgInfo
  buildVideoMessage' updateId chatId msgInfo videoInfo

buildVideoMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Telegram.Video ->
  Maybe PureStructs.PureMessage
buildVideoMessage' updateId chatId msgInfo videoInfo =
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
          <> [PureStructs.ParamsText "video" (Telegram.video_file_id videoInfo)]
