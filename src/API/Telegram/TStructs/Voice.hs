module API.Telegram.TStructs.Voice where

import Data.Aeson ( (.:), FromJSON(parseJSON), Value(Object) ) 
import qualified Data.Text as T 
import Data.Aeson.Types ( parseFail ) 
import API.Messages ( parseFailMessage )

data TelVoice = TelVoice
    {
        voice_file_id :: T.Text
    } deriving Show 

instance FromJSON TelVoice where 
    parseJSON (Object v) = 
        TelVoice <$> v .: "file_id"
    parseJSON _ = parseFail parseFailMessage