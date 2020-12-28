module API.Telegram.TStructs.Animation where

import Data.Aeson ( (.:), FromJSON(parseJSON), Value(Object) ) 
import qualified Data.Text as T 
import Data.Aeson.Types ( parseFail ) 
import API.Messages ( parseFailMessage )

data TelAmination = TelAmination 
    {
        animation_file_id :: T.Text
    } deriving Show 

instance FromJSON TelAmination where 
    parseJSON (Object v) =
        TelAmination <$> v .: "file_id"
    parseJSON _ = parseFail parseFailMessage