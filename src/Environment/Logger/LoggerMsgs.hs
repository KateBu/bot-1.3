module Environment.Logger.LoggerMsgs where

import qualified Environment.Logger.LoggerStructs as Logger

vkFatalError :: Logger.LogMessage
vkFatalError =
  Logger.LogMessage
    Logger.Error
    "FATAL ERROR: VK Config parsing error. Couldn't find group id or token"

runBot :: Logger.LogMessage
runBot =
  Logger.LogMessage
    Logger.Info
    "Bot is running..."

getRespMulti :: Logger.LogMessage
getRespMulti =
  Logger.LogMessage
    Logger.Debug
    "Trying to get response with multipart body..."

getRespUrl :: Logger.LogMessage
getRespUrl =
  Logger.LogMessage
    Logger.Debug
    "Trying to get response with url-encoded body..."

tDecBS :: Logger.LogMessage
tDecBS =
  Logger.LogMessage
    Logger.Debug
    "Telegram bot is trying to decode bytestring into TelegramUpdates..."

vkDecBS :: Logger.LogMessage
vkDecBS =
  Logger.LogMessage
    Logger.Debug
    "VK bot is trying to decode bytestring into VKUpdates..."

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
    Logger.Info
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

httpEx :: Logger.LogMessage
httpEx = Logger.LogMessage Logger.Error "Http exception: "

addUserQueryFld :: Logger.LogMessage
addUserQueryFld = Logger.LogMessage Logger.Error "Couldn't add user to Database"

findUserQueryFld :: Logger.LogMessage
findUserQueryFld = Logger.LogMessage Logger.Error "Find user query failed"

addUserRepeatScs :: Logger.LogMessage
addUserRepeatScs = Logger.LogMessage Logger.Info "New user was inserted into Database"

updUserRepeatNoUser :: Logger.LogMessage
updUserRepeatNoUser = Logger.LogMessage Logger.Warning "Cannot find userId in Database to update repeats"

updUserRepeatScs ::  Logger.LogMessage
updUserRepeatScs = Logger.LogMessage Logger.Info "The new repeat value for the user was set in Database"

updUserRepeatFld ::  Logger.LogMessage
updUserRepeatFld = Logger.LogMessage Logger.Error "UpdateUserRepeat query failed"

initLogFld ::  Logger.LogMessage
initLogFld = Logger.LogMessage Logger.Error "Cannot parse log priority from config file"

findUserScs :: Logger.LogMessage
findUserScs = Logger.LogMessage Logger.Debug "User was found in Database"

