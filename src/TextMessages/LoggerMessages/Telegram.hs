module TextMessages.LoggerMessages.Telegram where

import qualified Environment.Logger.Structs as Logger

chatIdNotFound :: Logger.LogMessage
chatIdNotFound =
  Logger.LogMessage
    Logger.Error
    "Chat ID not found"

telegramBytestringDecodingInProgress :: Logger.LogMessage
telegramBytestringDecodingInProgress =
  Logger.LogMessage
    Logger.Debug
    "Telegram bot is trying to decode bytestring into TelegramUpdates..."

telegramBytestringDecodingSuccess :: Logger.LogMessage
telegramBytestringDecodingSuccess =
  Logger.LogMessage
    Logger.Debug
    "Bytestring decoded into TelegramUpdates..."

getTelegramUpdatesSuccess :: Logger.LogMessage
getTelegramUpdatesSuccess =
  Logger.LogMessage
    Logger.Debug
    "Telegram bot got updates successfully..."

sendTelegramMsgSuccess :: Logger.LogMessage
sendTelegramMsgSuccess =
  Logger.LogMessage
    Logger.Debug
    "Telegram bot sent message successfully..."

parseTelelegramMsgSuccess :: Logger.LogMessage
parseTelelegramMsgSuccess =
  Logger.LogMessage
    Logger.Debug
    "Telegram messages were parsed to PureMessages successfully..."

noUpdates :: Logger.LogMessage
noUpdates =
  Logger.LogMessage
    Logger.Error
    "No updates were found"

telegramUpdatesFailed :: Logger.LogMessage
telegramUpdatesFailed =
  Logger.LogMessage
    Logger.Error
    "Telegram bot couldn't get updates"
