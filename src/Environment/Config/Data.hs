module Environment.Config.Data where

import qualified Data.Text as T

vkApiVersion :: T.Text
vkApiVersion = "5.126"

vkLongPollUrl :: String
vkLongPollUrl = "https://api.vk.com/method/groups.getLongPollServer?group_id="

timeOut :: T.Text
timeOut = "25"
