module API.Telegram.Functions.Attachments.Video where

import API.Telegram.Functions.BasicParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

buildVideoMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
buildVideoMessage updateId chatId msgInfo = do
  videoInfo <- TStructs.video msgInfo
  buildVideoMessage' updateId chatId msgInfo videoInfo

buildVideoMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelVideo ->
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
          <> [PureStructs.ParamsText "video" (TStructs.video_file_id videoInfo)]
