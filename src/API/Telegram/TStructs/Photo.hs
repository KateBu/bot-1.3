module API.Telegram.TStructs.Photo where

import Data.Aeson ( (.:), FromJSON(parseJSON), Value(Object) ) 
import qualified Data.Text as T 
import Data.Aeson.Types ( parseFail ) 
import API.Messages ( parseFailMessage )

data TelPhoto = TelPhoto 
    {
        photo_file_id :: T.Text
    } deriving Show

instance FromJSON TelPhoto where 
    parseJSON (Object v) = 
        TelPhoto <$> v .: "file_id"
    parseJSON _ = parseFail parseFailMessage