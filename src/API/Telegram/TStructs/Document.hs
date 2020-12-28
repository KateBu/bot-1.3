module API.Telegram.TStructs.Document where

import Data.Aeson ( (.:), FromJSON(parseJSON), Value(Object) ) 
import qualified Data.Text as T 
import Data.Aeson.Types ( parseFail ) 
import API.Messages ( parseFailMessage )

data TelDocument = TelDocument 
    {
         doc_file_id :: T.Text
    } deriving Show 

instance FromJSON TelDocument where 
    parseJSON (Object v) = 
        TelDocument <$> v .: "file_id"
    parseJSON _ = parseFail parseFailMessage