module TextMessages.LoggerMessages.VK where

import qualified Environment.Logger.Structs as Logger

vkUpdatesFailedCode1 :: Logger.LogMessage
vkUpdatesFailedCode1 =
  Logger.LogMessage
    Logger.Error
    "VKUpdate failed 1: Events was partly lost, use new ts value"

vkUpdatesFailedCode2 :: Logger.LogMessage
vkUpdatesFailedCode2 =
  Logger.LogMessage
    Logger.Error
    "VKUpdate failed 2: Key value is not acceptable anymore, get new key value"

vkUpdatesFailedCode3 :: Logger.LogMessage
vkUpdatesFailedCode3 =
  Logger.LogMessage
    Logger.Error
    "VKUpdate failed 3: Information was lost, get new key and ts values"

vkUpdatesFailedCode4 :: Logger.LogMessage
vkUpdatesFailedCode4 =
  Logger.LogMessage
    Logger.Error
    "VKUpdate failed: Unexpected error code"

vkUpdatesDecodingFailed :: Logger.LogMessage
vkUpdatesDecodingFailed =
  Logger.LogMessage
    Logger.Error
    "Decode vk update failed: "

vkMsgTypeNotImplemented :: Logger.LogMessage
vkMsgTypeNotImplemented =
  Logger.LogMessage
    Logger.Error
    "Pattern is not implemented yet"

unexpectedVKEvent :: Logger.LogMessage
unexpectedVKEvent =
  Logger.LogMessage
    Logger.Error
    "Unexpected VK event"

noChatId :: Logger.LogMessage
noChatId =
  Logger.LogMessage
    Logger.Error
    "Chat ID not found"

vkByteStringDecodingInProgress :: Logger.LogMessage
vkByteStringDecodingInProgress =
  Logger.LogMessage
    Logger.Debug
    "VK bot is trying to decode bytestring into VKUpdates..."

vkDecodeByteStringSuccess :: Logger.LogMessage
vkDecodeByteStringSuccess =
  Logger.LogMessage
    Logger.Debug
    "Bytestring decoded into VKUpdates..."

getVKUpdatesSuccess :: Logger.LogMessage
getVKUpdatesSuccess =
  Logger.LogMessage
    Logger.Debug
    "VK bot got updates successfully..."

sendVkMsgSuccess :: Logger.LogMessage
sendVkMsgSuccess =
  Logger.LogMessage
    Logger.Debug
    "VK bot sent message successfully..."

parseVKMsgSuccess :: Logger.LogMessage
parseVKMsgSuccess =
  Logger.LogMessage
    Logger.Debug
    "VK messages were parsed to PureMessages successfully..."
