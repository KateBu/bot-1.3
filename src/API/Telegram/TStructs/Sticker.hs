module API.Telegram.TStructs.Sticker where

import Data.Aeson ( (.:), FromJSON(parseJSON), Value(Object) ) 
import qualified Data.Text as T 
import Data.Aeson.Types ( parseFail ) 
import API.Messages ( parseFailMessage )

data TelSticker = TelSticker 
    {
        s_file_id :: T.Text
        , s_is_animated :: Bool 

    } deriving Show 

instance FromJSON TelSticker where 
    parseJSON (Object v) = 
        TelSticker <$> v .: "file_id"
        <*> v .: "is_animated"
    parseJSON _ = parseFail parseFailMessage