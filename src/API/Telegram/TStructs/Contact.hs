module API.Telegram.TStructs.Contact where

import Data.Aeson
    ( (.:), (.:?), FromJSON(parseJSON), Value(Object) ) 
import qualified Data.Text as T 
import Data.Aeson.Types ( parseFail ) 
import API.Messages ( parseFailMessage )

data TelContact = TelContact
    {
        phone_number :: T.Text
        , first_name :: T.Text
        , last_name :: Maybe T.Text
        , vcard :: Maybe T.Text
    } deriving (Show)

instance FromJSON TelContact where
    parseJSON (Object v) = 
        TelContact <$> v.: "phone_number"
            <*> v .: "first_name"
            <*> v .:? "last_name"
            <*> v .:? "vcard"
    parseJSON _ = parseFail parseFailMessage