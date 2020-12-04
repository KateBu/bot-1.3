module API.VK.Wrapper where

import Config.Config 
import Logic.PureStructs 

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T 
import qualified Data.Text.Lazy as TL 
import qualified Data.Text.Encoding as TLE 
import Network.HTTP.Simple 

--import qualified Data.ByteString as BC 
import qualified Data.ByteString.Lazy as BL 
import qualified Data.ByteString.Lazy.Char8 as BLC 
--import qualified Data.ByteString.Char8 as 


sendMessageHttpRequest :: Config -> String 
sendMessageHttpRequest (Config (VK tok _ _ _ _) _ _ _ _) = 
    "https://api.vk.com/method/messages.send?access_token="
    <> tok 
   -- <> "&v="
   -- <> vkApiVersion
  --  <> "&random_id="
  --  <> show random_id 
  --  <> "&user_id=10010924"
  --  <> "&message=hello"
{-
makeMessageObject :: Message ->  Value 
makeMessageObject (CommonMessage _ chid msg _)  = object $ 
    ["user_id" .= show chid]
    <> ["peer_id" .= chid]
   -- <> ["random_id" .= (0 :: Integer)]
    <> messageToPairs msg 
-}

makeRequestBody :: Message -> Integer -> String
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
{-
buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Query -> Request 
buildRequest token host method path  query = setRequestMethod method                         
                        $ setRequestHost host 
                        $ setRequestHeader "access_token" [token]
                        $ setRequestPath path 
                        $ setRequestQueryString query
                        $ setRequestSecure True
                        $ setRequestPort 443
                        $ defaultRequest-}

{-
vkApiHost :: BC.ByteString
vkApiHost = "api.vk.com"

request :: Config -> Query -> Request 
request ((VK tok group key serv ts) _ _ _ _) query = buildRequest tok vkApiHost "POST" "/method/messages.send" query 

-}