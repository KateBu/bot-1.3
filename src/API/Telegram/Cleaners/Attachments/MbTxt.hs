module API.Telegram.Cleaners.Attachments.MbTxt where

import API.Telegram.Cleaners.GetParams (basicParams)
import API.Telegram.Cleaners.Keyboard
  ( TButtons (TButtons),
    makeKeyboard,
  )
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs

mbTextMessage ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbTextMessage helpMsg updateId chatId msgInfo = do
  textInfo <- TStructs.txt msgInfo
  makeTxtMsg helpMsg updateId chatId msgInfo textInfo

makeTxtMsg ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  T.Text ->
  Maybe PureStructs.PureMessage
makeTxtMsg helpMsg updateId chatId msgInfo "/help" =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeUserCommand PureStructs.Help)
      updateId
      (Just chatId)
      helpParams
  where
    helpParams =
      Just $
        basicParams chatId msgInfo
          <> [PureStructs.ParamsText "text" helpMsg]
makeTxtMsg _ updateId chatId msgInfo "/repeat" =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeUserCommand PureStructs.Repeat)
      updateId
      (Just chatId)
      repeatParams
  where
    repeatParams =
      Just $
        basicParams chatId msgInfo
          <> [ PureStructs.ParamsJSON "reply_markup" keyboard,
               PureStructs.ParamsBool "one_time_keyboard" True
             ]
    keyboard = makeKeyboard $ map (map TButtons) PureStructs.buttons
makeTxtMsg _ updateId chatId msgInfo text =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Message")
      updateId
      (Just chatId)
      messageParams
  where
    messageParams =
      Just $
        basicParams chatId msgInfo
          <> [PureStructs.ParamsText "text" text]
