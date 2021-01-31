module Environment.Logger.Messages.VK where

import qualified Environment.Logger.Structs as Logger

vkUpdatesFailed1 :: Logger.LogMessage
vkUpdatesFailed1 =
  Logger.LogMessage
    Logger.Error
    "VKUpdate failed 1: Events was partly lost, use new ts value"

vkUpdatesFailed2 :: Logger.LogMessage
vkUpdatesFailed2 =
  Logger.LogMessage
    Logger.Error
    "VKUpdate failed 2: Key value is not acceptable anymore, get new key value"

vkUpdatesFailed3 :: Logger.LogMessage
vkUpdatesFailed3 =
  Logger.LogMessage
    Logger.Error
    "VKUpdate failed 3: Information was lost, get new key and ts values"

vkUpdatesFailed4 :: Logger.LogMessage
vkUpdatesFailed4 =
  Logger.LogMessage
    Logger.Error
    "VKUpdate failed: Unexpected error code"

vkDecUpdatesFailed :: Logger.LogMessage
vkDecUpdatesFailed =
  Logger.LogMessage
    Logger.Error
    "Decode vk update failed: "

notImplemented :: Logger.LogMessage
notImplemented =
  Logger.LogMessage
    Logger.Error
    "Pattern is not implemented yet"

unexpVKEvent :: Logger.LogMessage
unexpVKEvent =
  Logger.LogMessage
    Logger.Error
    "Unexpected VK event"

noChatId :: Logger.LogMessage
noChatId =
  Logger.LogMessage
    Logger.Error
    "Chat ID not found"

vkDecBS :: Logger.LogMessage
vkDecBS =
  Logger.LogMessage
    Logger.Debug
    "VK bot is trying to decode bytestring into VKUpdates..."

vkDecBsScs :: Logger.LogMessage
vkDecBsScs =
  Logger.LogMessage
    Logger.Debug
    "Bytestring decoded into VKUpdates..."

getVKUpdScs :: Logger.LogMessage
getVKUpdScs =
  Logger.LogMessage
    Logger.Debug
    "VK bot got updates successfully..."

sndMsgScsVK :: Logger.LogMessage
sndMsgScsVK =
  Logger.LogMessage
    Logger.Debug
    "VK bot sent message successfully..."

parseVKMsgScs :: Logger.LogMessage
parseVKMsgScs =
  Logger.LogMessage
    Logger.Debug
    "VK messages were parsed to PureMessages successfully..."
