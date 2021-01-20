module API.Telegram.Cleaners.Attachments.MbTxt where

import API.Telegram.Cleaners.GetParams (basicParams)
import API.Telegram.Cleaners.Keyboard
  ( TButtons (TButtons),
    makeKeyboard,
  )
import qualified API.Telegram.TStructs.MessageInfo as TStructs
import qualified Config.Config as Config
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

mbTextMessage ::
  Config.Config ->
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbTextMessage config uid chid mInfo =
  TStructs.txt mInfo >>= mkTxtMsg config uid chid mInfo

mkTxtMsg ::
  Config.Config ->
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  T.Text ->
  Maybe PureStructs.PureMessage
mkTxtMsg config uid chid mInfo "/help" = undefined 
 {- pure $
    PureStructs.PureMessage
      (PureStructs.MTUserCommand PureStructs.Help)
      uid
      (Just chid)
      ( Just $
          basicParams chid mInfo
            <> [PureStructs.ParamsText "text" (Config.helpMessage config)]
      )-}
mkTxtMsg _ uid chid mInfo "/repeat" =
  pure $
    PureStructs.PureMessage
      (PureStructs.MTUserCommand PureStructs.Repeat)
      uid
      (Just chid)
      ( Just $
          basicParams chid mInfo
            <> [ PureStructs.ParamsJSON "reply_markup" (makeKeyboard ((map (map TButtons)) PureStructs.buttons')),
                 PureStructs.ParamsBool "one_time_keyboard" True
               ]
      )
mkTxtMsg _ uid chid mInfo text =
  pure $
    PureStructs.PureMessage
      (PureStructs.MTCommon "Message")
      uid
      (Just chid)
      ( Just $
          basicParams chid mInfo
            <> [PureStructs.ParamsText "text" text]
      )
