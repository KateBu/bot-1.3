module API.VK.Parsers where

import API.VK.Structs

import Data.Aeson 


instance FromJSON VKUpdates where 
    parseJSON = withObject "VKUpdates" $ \obj -> do
        resp <- obj .:? "failed"
        case (resp :: Maybe Int) of 
            Just val -> VKUpdateError val <$> 
                obj .:? "ts"
            Nothing -> VKUpdates <$> --obj .: "ts"
                obj .: "updates"


{-

instance FromJSON EventType where 
    parseJSON = withObject "EventType" $ \obj -> do 
        upType <- obj .: "type" 
        case (upType :: String) of 
            "message_new" -> pure MsgNew 
            _ -> pure OtherEvent 
-}
instance FromJSON VKMessage where 
    parseJSON (Object obj) = 
        VKMessage <$> obj .: "id"
            <*> obj .: "peer_id"
            <*> obj .: "from_id"
            <*> obj .:? "text"

instance FromJSON VKUpdInfo where 
    parseJSON = withObject "VKUpdInfo" $ \obj -> do 
        upType <- obj .: "type"
        case (upType :: String) of 
            "message_new" -> VKUpdInfo MsgNew <$>
                obj .:? "object"
                <*> obj .:? "group_id"
            _ -> pure $ VKUpdInfo OtherEvent Nothing Nothing 





instance FromJSON VKResponse where 
    parseJSON = withObject "VKResponse" $ \obj -> do
        resp <- obj .:? "response"
        case resp of 
            Just val ->             
                VKResponse <$> val .: "key"
                <*> val .: "server"
                <*> val .: "ts"
            Nothing -> do 
                err <- obj .:? "error"
                case err of 
                    Just val -> 
                        VKError <$> val .: "error_code"
                        <*> val .: "error_msg"
                    Nothing -> 
                        pure VKParseError 




