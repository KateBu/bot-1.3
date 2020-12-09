module API.VK.Parsers where

import qualified API.VK.Structs as VKStructs

import Data.Aeson
    ( FromJSON(parseJSON), Value(Object), withObject, (.:), (.:?) ) 


instance FromJSON VKStructs.VKUpdates where 
    parseJSON = withObject "VKUpdates" $ \obj -> do
        resp <- obj .:? "failed"
        case (resp :: Maybe Int) of 
            Just val -> VKStructs.VKUpdateError val <$> 
                obj .:? "ts"
            Nothing -> VKStructs.VKUpdates <$> 
                obj .: "updates"

instance FromJSON VKStructs.VKMessage where 
    parseJSON (Object obj) = 
        VKStructs.VKMessage <$> obj .: "id"
            <*> obj .: "peer_id"
            <*> obj .: "from_id"
            <*> obj .:? "text"
            <*> obj .:? "attachments"

instance FromJSON VKStructs.Attachment where 
    parseJSON = withObject "Attachment" $ \obj -> do 
        attachType <- obj .: "type"
        case (attachType :: String) of 
            "photo" -> do
                photo <- obj .: "photo" 
                VKStructs.Attachment attachType <$> 
                    (VKStructs.VKPhoto <$> photo .: "id"
                    <*> photo .: "owner_id"
                    <*> photo .: "text"
                    <*> photo .: "access_key")
            _ -> pure VKStructs.UnknownAttachment 

instance FromJSON VKStructs.VKUpdInfo where 
    parseJSON = withObject "VKUpdInfo" $ \obj -> do 
        upType <- obj .: "type"
        case (upType :: String) of 
            "message_new" -> VKStructs.VKUpdInfo VKStructs.MsgNew <$>
                obj .:? "object"
                <*> obj .:? "group_id"
            _ -> pure $ VKStructs.VKUpdInfo VKStructs.OtherEvent Nothing Nothing 

instance FromJSON VKStructs.VKObject where 
    parseJSON (Object obj) = VKStructs.VKObject <$> obj .: "message"
        <*> obj .: "client_info"

instance FromJSON VKStructs.ClientInfo where 
    parseJSON (Object obj) = VKStructs.ClientInfo <$> obj .: "button_actions" 
        <*> obj .: "keyboard"
        <*> obj .: "inline_keyboard"

instance FromJSON VKStructs.VKResult where 
    parseJSON = withObject "VKResult" $ \obj -> do 
        isError <- obj .:? "error"
        case isError of 
            Just val -> pure $ VKStructs.SendMsgError val 
            Nothing -> VKStructs.SendMsgScs <$> obj .: "response"

instance FromJSON VKStructs.VKResultError where 
    parseJSON (Object obj) = VKStructs.VKResultError <$> 
        obj .: "error_code"
        <*> obj .: "error_msg"

instance FromJSON VKStructs.VKResponse where 
    parseJSON = withObject "VKResponse" $ \obj -> do
        resp <- obj .:? "response"
        case resp of 
            Just val ->             
                VKStructs.VKResponse <$> val .: "key"
                <*> val .: "server"
                <*> val .: "ts"
            Nothing -> do 
                err <- obj .:? "error"
                case err of 
                    Just val -> 
                        VKStructs.VKError <$> val .: "error_code"
                        <*> val .: "error_msg"
                    Nothing -> 
                        pure VKStructs.VKParseError 


