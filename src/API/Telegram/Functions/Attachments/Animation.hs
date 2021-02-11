module API.Telegram.Functions.Attachments.Animation (buildAnimationMessage) where

import API.Telegram.Functions.Params (basicParams)
import qualified API.Telegram.Structs.MessageInfo as Telegram
import qualified Logic.Structs as PureStructs

buildAnimationMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Maybe PureStructs.PureMessage
buildAnimationMessage updateId chatId msgInfo = do
  animationInfo <- Telegram.animation msgInfo
  buildAnimationMessage' updateId chatId msgInfo animationInfo

buildAnimationMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Telegram.Amination ->
  Maybe PureStructs.PureMessage
buildAnimationMessage' updateId chatId msgInfo animationInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Animation")
      updateId
      (Just chatId)
      animParams
  where
    animParams =
      Just $
        basicParams chatId msgInfo
          <> [PureStructs.ParamsText "animation" (Telegram.animation_file_id animationInfo)]
