module API.VK.Wrapper where

import Config.Config 
import Logic.PureStructs 

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T 

sendMessageHttpRequest :: Config -> String 
sendMessageHttpRequest (Config (VK tok group _ _ _) _ _ _ _) = 
    "https://api.vk.com/method/messages.send?group_id=" 
    <> show group 
    <> "access_token="
    <> tok 

makeMessageObject :: Message -> Value 
makeMessageObject (CommonMessage _ chid msg _) = object $ 
    ["user_id" .= chid]
    <> ["peer_id" .= chid]
    <> ["random_id" .= show 0]
    <> messageToPairs msg 



messageToPairs :: CMessage -> [Pair]
messageToPairs (Txt txt ) = ["message" .= txt]



getMaybeText :: T.Text -> Maybe T.Text -> [Pair] 
getMaybeText prop (Just val) = [prop .= val]
getMaybeText _ _ = []

getMaybeVal :: Show a => T.Text -> Maybe a -> [Pair]
getMaybeVal prop (Just val) = [prop .= show val]
getMaybeVal _ _ = [] 