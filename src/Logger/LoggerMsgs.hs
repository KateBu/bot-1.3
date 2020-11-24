module Logger.LoggerMsgs where

import qualified Data.Text as T 
import qualified Data.Map as Map 
import Data.Maybe 

logMsgs :: Map.Map String T.Text 
logMsgs = Map.fromList [
    ("nothing", "processMessage: nothing to process")
    , ("emptyMsgScs", "Empty message processed successfully: ")
    , ("emptyMsgFld", "Empty message process failed: ")
    , ("comMsgScs", "Common message processed successfully: ")
    , ("comMsgFld", "Common message process failed: ")
    , ("hlpMsgScs", "Help message processed successfully: ")
    , ("hlpMsgFld", "Help message process failed: ")
    , ("rptMsgScs", "Repeat message processed successfully: ")
    , ("rptMsgFld", "Repeat message process failed: ")
    , ("getUpdScs", "GetUpdates fuction processed successfully! ")
    , ("getUpdFld", "GetUpdates function failed: ")
    , ("prsMsgScs", "parseMessage function processed successfully: ")
    , ("prsMsgFld", "parseMessage function failed: ")
    , ("sndMsgScs", "sendMessage function processed successfully: ")
    , ("sndMsgFld", "sendMessage function failed: ")
    , ("prsConfScs", "Config parsed successfully: ")
    , ("prsConfFld", "Config parse failed: ")
    , ("tUpdConvert", "Telegram Updates successfully converted to [Message]")
    , ("vkUpdConvert", "VK Updates successfully converted to [Message]")
    ]

getLogMsg :: String -> T.Text
getLogMsg key = case Map.lookup key logMsgs of 
    Nothing -> "getLogMsg: Key not found"
    Just value -> value 

