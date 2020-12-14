module API.VK.Wrapper where

import qualified Data.Text as T 

import qualified Config.Config as Config 
import qualified Logic.PureStructs as PureStructs 


-- this file will be removed soon 


sendMessageHttpRequest :: Config.Config -> String 
sendMessageHttpRequest (Config.Config (Config.VK tok _ _ _ _) _ _ _ _) = 
    "https://api.vk.com/method/messages.send?access_token="
    <> tok 
sendMessageHttpRequest _ = ""

makeRequestBody :: PureStructs.Message -> Int -> String
makeRequestBody (PureStructs.CommonMessage _ chid msg _) randomId = mconcat
    ["&v=", Config.vkApiVersion
    , "&user_id=", show chid
    , "&random_id=", show randomId
    , msgToBL msg
    , makeInlineKeyboard (PureStructs.buttons msg)
    ]
makeRequestBody _ _ = undefined 

msgToBL :: PureStructs.ComMessage -> String
msgToBL cMsg = getMaybeText (PureStructs.mbText cMsg)

getMaybeText :: Maybe T.Text -> String
getMaybeText Nothing = ""
getMaybeText (Just txt) = T.unpack ("&message=" <> txt)

makeInlineKeyboard :: Bool -> String
makeInlineKeyboard False = ""
makeInlineKeyboard _ = undefined 
