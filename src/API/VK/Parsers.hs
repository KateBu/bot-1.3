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

instance FromJSON VKObject where 
    parseJSON (Object obj) = VKObject <$> obj .: "message"
        <*> obj .: "client_info"

instance FromJSON ClientInfo where 
    parseJSON (Object obj) = ClientInfo <$> obj .: "button_actions" 
        <*> obj .: "keyboard"
        <*> obj .: "inline_keyboard"



instance FromJSON VKResult where 
    parseJSON = withObject "VKResult" $ \obj -> do 
        isError <- obj .: "error"
        case isError of 
            Just val -> pure $ SendMsgError val 
            Nothing -> SendMsgScs <$> obj .: "response"


instance FromJSON VKResultError where 
    parseJSON (Object obj) = VKResultError <$> 
        obj .: "error_code"
        <*> obj .: "error_msg"


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




