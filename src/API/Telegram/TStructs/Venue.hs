module API.Telegram.TStructs.Venue where

import Data.Aeson ( (.:), FromJSON(parseJSON), Value(Object) ) 
import qualified Data.Text as T 
import Data.Aeson.Types ( parseFail ) 
import API.Messages ( parseFailMessage )

data TelVenue = TelVenue
    {
        v_latitude :: Double 
        , v_longitude :: Double
        , v_title :: T.Text
        , v_address :: T.Text
    } deriving Show 

instance FromJSON TelVenue where 
    parseJSON (Object v) = 
        TelVenue <$> v .: "latitude"
        <*> v .: "longitude"
        <*> v .: "title"
        <*> v .: "address"
    parseJSON _ = parseFail parseFailMessage