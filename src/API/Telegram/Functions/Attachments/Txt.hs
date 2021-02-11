module API.Telegram.Functions.Attachments.Txt (buildTextMessage) where

import qualified API.PureStructs.Exports as PureStructs
import API.Telegram.Functions.Params (buildBasicParams)
import qualified API.Telegram.Structs.Attachments.Keyboard as Telegram
import qualified API.Telegram.Structs.MessageInfo as Telegram
import Data.Aeson (KeyValue ((.=)), Value, object)
import qualified Data.Text as T
import qualified Environment.Exports as Env

buildTextMessage ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Maybe PureStructs.PureMessage
buildTextMessage helpMsg updateId chatId msgInfo = do
  textInfo <- Telegram.txt msgInfo
  buildTxtMsg helpMsg updateId chatId msgInfo textInfo

buildTxtMsg ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
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
        buildBasicParams chatId msgInfo
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
        buildBasicParams chatId msgInfo
          <> [ PureStructs.ParamsJSON "reply_markup" keyboard,
               PureStructs.ParamsBool "one_time_keyboard" True
             ]
    keyboard = makeKeyboard $ map (map Telegram.Buttons) PureStructs.buttons
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
        buildBasicParams chatId msgInfo
          <> [PureStructs.ParamsText "text" text]

makeKeyboard :: [[Telegram.Buttons]] -> Value
makeKeyboard buttons = object ["inline_keyboard" .= buttons]
