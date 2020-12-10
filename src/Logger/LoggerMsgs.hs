module Logger.LoggerMsgs where

import qualified Data.Text as T 
import qualified Logger.Logger as Logger 


fatalConfig :: T.Text
fatalConfig = "FATAL ERROR: couldn't find or read the config file," 
            <> " check the path to the file and the information inside it"

parseVKConfFld :: Logger.LogMessage
parseVKConfFld = Logger.LogMessage Logger.Error 
    "Parse VK Config Failed: "

parseVKConfNoInfo :: Logger.LogMessage
parseVKConfNoInfo = Logger.LogMessage Logger.Error 
    "Parse VK Config Failed: no information"

emptyList :: Logger.LogMessage
emptyList = Logger.LogMessage Logger.Debug 
    "Empty list: nothing to process"

emptyMsgScs :: Logger.LogMessage
emptyMsgScs = Logger.LogMessage Logger.Debug 
    "Empty message processed successfully!"

emptyMsgFld :: Logger.LogMessage
emptyMsgFld = Logger.LogMessage Logger.Error 
    "Empty message process failed: "

cmdMsgFld :: Logger.LogMessage
cmdMsgFld = Logger.LogMessage Logger.Error 
    "Command message process failed: "

comMsgScs :: Logger.LogMessage
comMsgScs = Logger.LogMessage Logger.Debug 
    "Common message processed successfully!"

comMsgFld :: Logger.LogMessage
comMsgFld = Logger.LogMessage Logger.Error 
    "Common message process failed: unknown command"

hlpMsgScs :: Logger.LogMessage
hlpMsgScs = Logger.LogMessage Logger.Debug 
    "Help message processed successfully!"

hlpMsgFld :: Logger.LogMessage
hlpMsgFld = Logger.LogMessage Logger.Error 
    "Help message process failed: "

rptMsgScs :: Logger.LogMessage
rptMsgScs = Logger.LogMessage Logger.Debug 
    "Repeat message processed successfully!"

rptMsgFld :: Logger.LogMessage
rptMsgFld = Logger.LogMessage Logger.Error 
    "Repeat message process failed: "

getUpdScs :: Logger.LogMessage
getUpdScs = Logger.LogMessage Logger.Debug 
    "GetUpdates fuction processed successfully!"

getUpdFld :: Logger.LogMessage
getUpdFld = Logger.LogMessage Logger.Error 
    "GetUpdates function failed: "

prsMsgScs :: Logger.LogMessage
prsMsgScs = Logger.LogMessage Logger.Debug 
    "parseMessage function processed successfully!"

prsMsgFld :: Logger.LogMessage
prsMsgFld = Logger.LogMessage Logger.Error 
    "parseMessage function failed: "

sndMsgScs :: Logger.LogMessage
sndMsgScs = Logger.LogMessage Logger.Debug 
    "sendMessage function processed successfully!"

sndMsgFld :: Logger.LogMessage
sndMsgFld = Logger.LogMessage Logger.Error 
    "sendMessage function failed: "

prsConfScs :: Logger.LogMessage
prsConfScs = Logger.LogMessage Logger.Debug 
    "Config parsed successfully!"

prsConfFld :: Logger.LogMessage
prsConfFld = Logger.LogMessage Logger.Error 
    "Config parse failed: "

noUpd :: Logger.LogMessage
noUpd = Logger.LogMessage Logger.Debug 
    "No updates to recieve"

chidNotFound :: Logger.LogMessage
chidNotFound = Logger.LogMessage Logger.Error 
    "Chat ID not found"

nextLoop :: Logger.LogMessage
nextLoop = Logger.LogMessage Logger.Debug 
    "Go to the next loop"

vkUpdatesFailed1 :: Logger.LogMessage
vkUpdatesFailed1 = Logger.LogMessage Logger.Warning 
    "VKUpdate failed 1: Events was partly lost, use new ts value"

vkUpdatesFailed2 :: Logger.LogMessage
vkUpdatesFailed2 = Logger.LogMessage Logger.Warning 
    "VKUpdate failed 2: Key value is not acceptable anymore, get new key value"

vkUpdatesFailed3 :: Logger.LogMessage
vkUpdatesFailed3 = Logger.LogMessage Logger.Warning 
    "VKUpdate failed 3: Information was lost, get new key and ts values"

vkUpdatesFailed4 :: Logger.LogMessage
vkUpdatesFailed4 = Logger.LogMessage Logger.Error  
    "VKUpdate failed: Unexpected error code"

vkUpdatesSuccess :: Logger.LogMessage
vkUpdatesSuccess = Logger.LogMessage Logger.Debug 
    "VK updates revieved successfully"

vkUpdatesParsingUnknownMsgType :: Logger.LogMessage
vkUpdatesParsingUnknownMsgType = Logger.LogMessage Logger.Error 
    "Cannot parse VK updates into Pure Message, unknonw VK updates event type"

vkUpdatesParsingNoMsg :: Logger.LogMessage
vkUpdatesParsingNoMsg = Logger.LogMessage Logger.Error 
    "VK updates message not found"

vkUpdatesParsingNoGroupID :: Logger.LogMessage
vkUpdatesParsingNoGroupID = Logger.LogMessage Logger.Error 
    "VK updates group ID not found"

unreadableConfig :: Logger.LogMessage
unreadableConfig = Logger.LogMessage Logger.Error      
    "Got unreadeble config"
