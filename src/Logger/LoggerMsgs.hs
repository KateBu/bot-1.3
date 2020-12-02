module Logger.LoggerMsgs where

import qualified Data.Text as T 
import Logger.Logger 

fatalConfig :: T.Text
fatalConfig = "FATAL ERROR: couldn't find or read the config file," 
            <> " check the path to the file and the information inside it"

parseVKConfFld :: LogMessage
parseVKConfFld = LogMessage Error "Parse VK Config Failed: "

parseVKConfNoInfo :: LogMessage
parseVKConfNoInfo = LogMessage Error "Parse VK Config Failed: no information"

emptyList :: LogMessage
emptyList = LogMessage Debug "Empty list: nothing to process"

emptyMsgScs :: LogMessage
emptyMsgScs = LogMessage Debug "Empty message processed successfully!"

emptyMsgFld :: LogMessage
emptyMsgFld = LogMessage Error "Empty message process failed: "

cmdMsgFld :: LogMessage
cmdMsgFld = LogMessage Error "Command message process failed: "

comMsgScs :: LogMessage
comMsgScs = LogMessage Debug "Common message processed successfully!"

comMsgFld :: LogMessage
comMsgFld = LogMessage Error "Common message process failed: unknown command"

hlpMsgScs :: LogMessage
hlpMsgScs = LogMessage Debug "Help message processed successfully!"

hlpMsgFld :: LogMessage
hlpMsgFld = LogMessage Error "Help message process failed: "

rptMsgScs :: LogMessage
rptMsgScs = LogMessage Debug "Repeat message processed successfully!"

rptMsgFld :: LogMessage
rptMsgFld = LogMessage Error "Repeat message process failed: "

getUpdScs :: LogMessage
getUpdScs = LogMessage Debug "GetUpdates fuction processed successfully!"

getUpdFld :: LogMessage
getUpdFld = LogMessage Error "GetUpdates function failed: "

prsMsgScs :: LogMessage
prsMsgScs = LogMessage Debug "parseMessage function processed successfully!"

prsMsgFld :: LogMessage
prsMsgFld = LogMessage Error "parseMessage function failed: "

sndMsgScs :: LogMessage
sndMsgScs = LogMessage Debug "sendMessage function processed successfully!"

sndMsgFld :: LogMessage
sndMsgFld = LogMessage Error "sendMessage function failed: "

prsConfScs :: LogMessage
prsConfScs = LogMessage Debug "Config parsed successfully!"

prsConfFld :: LogMessage
prsConfFld = LogMessage Error "Config parse failed: "

noUpd :: LogMessage
noUpd = LogMessage Debug "No updates to recieve"

chidNotFound :: LogMessage
chidNotFound = LogMessage Error "Chat ID not found"

nextLoop :: LogMessage
nextLoop = LogMessage Debug "Go to the next loop"


vkUpdatesFailed1 :: LogMessage
vkUpdatesFailed1 = LogMessage Warning "VKUpdate failed 1: Events was partly lost, use new ts value"

vkUpdatesFailed2 :: LogMessage
vkUpdatesFailed2 = LogMessage Warning "VKUpdate failed 2: Key value is not acceptable anymore, get new key value"

vkUpdatesFailed3 :: LogMessage
vkUpdatesFailed3 = LogMessage Warning "VKUpdate failed 3: Information was lost, get new key and ts values"

vkUpdatesSuccess :: LogMessage
vkUpdatesSuccess = LogMessage Debug "VK updates revieved successfully"

vkUpdatesParsingUnknownMsgType :: LogMessage
vkUpdatesParsingUnknownMsgType = LogMessage Error 
    "Cannot parse VK updates into Pure Message, unknonw VK updates event type"

vkUpdatesParsingNoMsg :: LogMessage
vkUpdatesParsingNoMsg = LogMessage Error "VK updates message not found"

vkUpdatesParsingNoGroupID :: LogMessage
vkUpdatesParsingNoGroupID = LogMessage Error "VK updates group ID not found"