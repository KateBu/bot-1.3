module API.VK.VKStructs.SendResult where

import qualified Data.Text as T 
import Data.Aeson
    ( FromJSON(parseJSON), (.:), (.:?), withObject, Value(Object) )
import Data.Aeson.Types ( parseFail )
import API.Messages ( parseFailMessage )

data VKResult = SendMsgScs SendSuccess
    | SendMsgError SendError
    deriving Show 

data SendSuccess = SendSuccess {
        newTs :: Int
    }  deriving Show 

data SendError = SendError {
        resError :: VKResultError
    } deriving Show 

instance FromJSON VKResult where 
    parseJSON = withObject "VKResult" $ \obj -> do 
        isError <- obj .:? "error"
        case isError of 
            Just val -> pure $ SendMsgError (SendError val) 
            Nothing -> SendMsgScs <$> (SendSuccess <$> obj .: "response")

data VKResultError = VKResultError {
    errCode :: Int
    , errMsg :: T.Text
} deriving Show 

instance FromJSON VKResultError where 
    parseJSON (Object obj) = VKResultError <$> 
        obj .: "error_code"
        <*> obj .: "error_msg"
    parseJSON _ = parseFail parseFailMessage