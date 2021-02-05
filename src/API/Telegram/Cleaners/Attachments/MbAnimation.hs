module API.Telegram.Cleaners.Attachments.MbAnimation where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbAnimation ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbAnimation updateId chatId msgInfo = do
  animationInfo <- TStructs.animation msgInfo
  mbAnimation' updateId chatId msgInfo animationInfo

mbAnimation' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelAmination ->
  Maybe PureStructs.PureMessage
mbAnimation' updateId chatId msgInfo animationInfo =
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
