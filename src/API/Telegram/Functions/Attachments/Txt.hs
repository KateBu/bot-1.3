module API.Telegram.Functions.Attachments.Txt where

import API.Telegram.Functions.BasicParams (basicParams)
import API.Telegram.Structs.Attachments.Keyboard (TButtons (TButtons))
import qualified API.Telegram.Structs.MessageInfo as TStructs
import Data.Aeson (KeyValue ((.=)), Value, object)
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs

buildTextMessage ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
buildTextMessage helpMsg updateId chatId msgInfo = do
  textInfo <- TStructs.txt msgInfo
  buildTxtMsg helpMsg updateId chatId msgInfo textInfo

buildTxtMsg ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  T.Text ->
  Maybe PureStructs.PureMessage
buildTxtMsg helpMsg updateId chatId msgInfo "/help" =
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
buildTxtMsg _ updateId chatId msgInfo "/repeat" =
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
buildTxtMsg _ updateId chatId msgInfo text =
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

makeKeyboard :: [[TButtons]] -> Value
makeKeyboard buttons = object ["inline_keyboard" .= buttons]
