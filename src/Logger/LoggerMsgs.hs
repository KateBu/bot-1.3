module Logger.LoggerMsgs where

import qualified Data.Text as T
import qualified Logger.Logger as Logger

fatalConfig :: T.Text
fatalConfig =
  "FATAL ERROR: couldn't find or read the config file,"
    <> " check the path to the file and the information inside it"

vkFatalError :: Logger.LogMessage
vkFatalError =
  Logger.LogMessage
    Logger.Error
    "FATAL ERROR: VK Config parsing error. Couldn't find group id or token"

getTelUpdScs :: Logger.LogMessage
getTelUpdScs =
  Logger.LogMessage
    Logger.Debug
    "Telegram bot got updates successfully!"

getVKUpdScs :: Logger.LogMessage
getVKUpdScs =
  Logger.LogMessage
    Logger.Debug
    "VK bot got updates successfully!"

sndMsgScsVK :: Logger.LogMessage
sndMsgScsVK =
  Logger.LogMessage
    Logger.Debug
    "VK bot sent message successfully"

sndMsgScsTel :: Logger.LogMessage
sndMsgScsTel =
  Logger.LogMessage
    Logger.Debug
    "Telegram bot sent message successfully"

getUpdFld :: Logger.LogMessage
getUpdFld =
  Logger.LogMessage
    Logger.Error
    "GetUpdates function failed: "

sndMsgFld :: Logger.LogMessage
sndMsgFld =
  Logger.LogMessage
    Logger.Error
    "sendMessage function failed: "

chidNotFound :: Logger.LogMessage
chidNotFound =
  Logger.LogMessage
    Logger.Error
    "Chat ID not found"

nextLoop :: Logger.LogMessage
nextLoop =
  Logger.LogMessage
    Logger.Debug
    "Go to the next loop"

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

invalidHP :: Logger.LogMessage
invalidHP =
  Logger.LogMessage
    Logger.Error
    "Invalid host path"

noUpd :: Logger.LogMessage
noUpd =
  Logger.LogMessage
    Logger.Error
    "No update was found"

badServerResponse :: Logger.LogMessage
badServerResponse =
  Logger.LogMessage
    Logger.Error
    "Got bad server response, response status code: "

parseVKMsgScs :: Logger.LogMessage
parseVKMsgScs =
  Logger.LogMessage
    Logger.Debug
    "VK messages were parsed to PureMessages successfully"

testError :: Logger.LogMessage
testError =
  Logger.LogMessage
    Logger.Debug
    "Test error"

initConfigExcept :: Logger.LogMessage
initConfigExcept =
  Logger.LogMessage
    Logger.Error
    ( "Couldn't find or read the config file,"
        <> " check the path to the file and the information inside it"
    )

parseErr :: Logger.LogMessage
parseErr = Logger.LogMessage Logger.Error "Parsing bytestring into struct failed: "
