module API.Telegram.TStructs.Video where

import Data.Aeson ( (.:), FromJSON(parseJSON), Value(Object) ) 
import qualified Data.Text as T 
import Data.Aeson.Types ( parseFail ) 
import API.Messages ( parseFailMessage )

data TelVideo = TelVideo 
    {
        video_file_id :: T.Text
    } deriving Show 

instance FromJSON TelVideo where 
    parseJSON (Object v) = 
        TelVideo <$> v .: "file_id"
    parseJSON _ = parseFail parseFailMessage