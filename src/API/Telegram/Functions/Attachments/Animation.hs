module API.Telegram.Functions.Attachments.Animation (buildAnimationMessage) where

import API.Telegram.Functions.Params (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.Structs as PureStructs

buildAnimationMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
buildAnimationMessage updateId chatId msgInfo = do
  animationInfo <- TStructs.animation msgInfo
  buildAnimationMessage' updateId chatId msgInfo animationInfo

buildAnimationMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelAmination ->
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
          <> [PureStructs.ParamsText "animation" (TStructs.animation_file_id animationInfo)]
