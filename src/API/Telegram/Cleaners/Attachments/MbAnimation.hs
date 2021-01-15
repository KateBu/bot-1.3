module API.Telegram.Cleaners.Attachments.MbAnimation where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.TStructs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbAnimation ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbAnimation uid chid mInfo =
  TStructs.animation mInfo >>= mbAnimation' uid chid mInfo

mbAnimation' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelAmination ->
  Maybe PureStructs.PureMessage
mbAnimation' uid chid mInfo anim =
  pure $
    PureStructs.PureMessage
      (PureStructs.MTCommon "Animation")
      uid
      (Just chid)
      ( Just $
          basicParams chid mInfo
            <> [PureStructs.ParamsText "animation" (TStructs.animation_file_id anim)]
      )
