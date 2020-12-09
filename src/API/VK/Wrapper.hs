module API.VK.Wrapper where

import Config.Config 
import Logic.PureStructs 

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T 
import qualified Data.Text.Lazy as TL 
import qualified Data.Text.Encoding as TLE 
import Network.HTTP.Simple 
import Network.HTTP.Client.Conduit

import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BL 
import qualified Data.ByteString.Lazy.Char8 as BLC 
import qualified Data.ByteString.Char8 as BSC 


sendMessageHttpRequest :: Config -> String 
sendMessageHttpRequest (Config (VK tok _ _ _ _) _ _ _ _) = 
    "https://api.vk.com/method/messages.send?access_token="
    <> tok 


makeRequestBody :: Message -> Int -> String
makeRequestBody (CommonMessage _ chid msg _) randomId = mconcat
    ["&v=", vkApiVersion
    , "&user_id=", show chid
    , "&random_id=", show randomId
    , msgToBL msg]

msgToBL :: CMessage -> String
msgToBL (Txt txt)= T.unpack ("&message=" <> txt)


messageToPairs :: CMessage -> [Pair]
messageToPairs (Txt txt ) = ["message" .= txt]



getMaybeText :: T.Text -> Maybe T.Text -> [Pair] 
getMaybeText prop (Just val) = [prop .= val]
getMaybeText _ _ = []

getMaybeVal :: Show a => T.Text -> Maybe a -> [Pair]
getMaybeVal prop (Just val) = [prop .= show val]
getMaybeVal _ _ = [] 
