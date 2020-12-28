module API.Telegram.TStructs.Chat where

import Data.Aeson ( (.:), FromJSON(parseJSON), Value(Object) ) 
import qualified Data.Text as T 
import Data.Aeson.Types ( parseFail ) 
import API.Messages ( parseFailMessage )

data TelChat = TelChat
    {
        chat_id :: Int
        ,chat_type :: T.Text 
    } deriving Show 

instance FromJSON TelChat where 
    parseJSON (Object v) = 
        TelChat <$> v .: "id"
            <*> v .: "type"
    parseJSON _ = parseFail parseFailMessage