module TextMessages.LoggerMessages.Telegram where

import qualified Environment.Logger.Structs as Logger

chidNotFound :: Logger.LogMessage
chidNotFound =
  Logger.LogMessage
    Logger.Error
    "Chat ID not found"

tDecBS :: Logger.LogMessage
tDecBS =
  Logger.LogMessage
    Logger.Debug
    "Telegram bot is trying to decode bytestring into TelegramUpdates..."

telDecBsScs :: Logger.LogMessage
telDecBsScs =
  Logger.LogMessage
    Logger.Debug
    "Bytestring decoded into TelegramUpdates..."

getTelUpdScs :: Logger.LogMessage
getTelUpdScs =
  Logger.LogMessage
    Logger.Debug
    "Telegram bot got updates successfully..."

sndMsgScsTel :: Logger.LogMessage
sndMsgScsTel =
  Logger.LogMessage
    Logger.Debug
    "Telegram bot sent message successfully..."

parseTelMsgScs :: Logger.LogMessage
parseTelMsgScs =
  Logger.LogMessage
    Logger.Debug
    "Telegram messages were parsed to PureMessages successfully..."
