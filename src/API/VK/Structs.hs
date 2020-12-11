module API.VK.Structs where

import qualified Data.Text as T 
import Data.Aeson
    ( (.:), 
    (.:?), 
    withObject, 
    object,
    FromJSON(parseJSON), 
    Value(Object),
    KeyValue((.=)),
    ToJSON(toJSON) ) 
import Data.Aeson.Types ( parseFail )
import GHC.Generics ( Generic )


parseFailMessage :: String
parseFailMessage = "VK bot got unexpected imput data type while parsing JSON"

data VKUpdates = VKUpdates {
        updates :: [VKUpdInfo]
    } 
    | VKUpdateError {
        failed :: Int
        , curTs :: Maybe Int
    } deriving (Show)

instance FromJSON VKUpdates where 
    parseJSON = withObject "VKUpdates" $ \obj -> do
        resp <- obj .:? "failed"
        case (resp :: Maybe Int) of 
            Just val -> VKUpdateError val <$> 
                obj .:? "ts"
            Nothing -> VKUpdates <$> 
                obj .: "updates" 

data VKUpdInfo = VKUpdInfo
    {
        updType :: EventType
        , updObj :: Maybe VKObject
        , groupId :: Maybe Int
    } deriving Show 

instance FromJSON VKUpdInfo where 
    parseJSON = withObject "VKUpdInfo" $ \obj -> do 
        upType <- obj .: "type"
        case (upType :: String) of 
            "message_new" -> VKUpdInfo MsgNew <$>
                obj .:? "object"
                <*> obj .:? "group_id"
            _ -> pure $ VKUpdInfo OtherEvent Nothing Nothing 

data VKObject = VKObject {
    vkMessage :: VKMessage
    , clientInfo :: ClientInfo
} deriving Show

instance FromJSON VKObject where 
    parseJSON (Object obj) = VKObject <$> obj .: "message"
        <*> obj .: "client_info"
    parseJSON _ = parseFail parseFailMessage

data ClientInfo = ClientInfo {
    vkButtonActions :: [String]
    , vkKeyboard :: Bool
    , vkInlineKeyboard :: Bool
}deriving Show

instance FromJSON ClientInfo where 
    parseJSON (Object obj) = ClientInfo <$> obj .: "button_actions" 
        <*> obj .: "keyboard"
        <*> obj .: "inline_keyboard"
    parseJSON _ = parseFail parseFailMessage

data EventType = MsgNew | OtherEvent 
    deriving Show 

data VKMessage = VKMessage 
    {
        id :: Integer
    --    , date :: Integer 
        , peer_id :: Int --reciever id 
        , from_id :: Int --sender id
        , msgText :: Maybe T.Text
     --   , random_id :: Maybe Integer
     --   , ref :: Maybe String 
     --   , ref_source :: Maybe String
        , attachments :: Maybe [Attachment]
     --   , important :: Maybe Bool
     {-   , geo :: Maybe Geo
        , payload :: Maybe String 
        , keyboard :: Maybe Keyboard 
        , fwd_messages :: Maybe [VKMessage]
        , reply_message :: Maybe VKMessage
        , action :: Maybe Action 
        , admin_author_id ::Maybe Integer 
        , conversation_message_id :: Maybe Integer
        , is_cropped :: Maybe Bool
        , members_count :: Maybe Integer 
        , update_time :: Maybe Integer 
        , was_listened :: Maybe Bool 
        , pinned_at :: Maybe Bool -} 
    } deriving Show 

instance FromJSON VKMessage where 
    parseJSON (Object obj) = 
        VKMessage <$> obj .: "id"
            <*> obj .: "peer_id"
            <*> obj .: "from_id"
            <*> obj .:? "text"
            <*> obj .:? "attachments"
    parseJSON _ = parseFail parseFailMessage

data Attachment = Attachment 
    {
        aType :: String 
        , aObject :: AObject
    } | UnknownAttachment
    deriving Show 

instance FromJSON Attachment where 
    parseJSON = withObject "Attachment" $ \obj -> do 
        attachType <- obj .: "type"
        case (attachType :: String) of 
            "photo" -> do
                photo <- obj .: "photo" 
                Attachment attachType <$> 
                    (VKPhoto <$> photo .: "id"
                    <*> photo .: "owner_id"
                    <*> photo .: "text"
                    <*> photo .: "access_key")
            _ -> pure UnknownAttachment 

data AObject = 
    VKPhoto 
        {
            phId :: Int
           -- , albumId :: Integer 
            , ownerId :: Int 
           -- , userId :: Integer 
            , phText :: T.Text
            , accessID :: T.Text
            --, phDate :: Integer 
           -- , phSizes :: [PhSizes]
           -- , phWidth :: Integer 
           -- , phHeight :: Integer 
        } deriving Show 

{-
data PhSizes = PhSizes
    {
        url :: String 
        , sWidth :: Int 
        , sHeight :: Int 
        , sType :: String 
    } deriving Show 
-}

data Geo = Geo 
    {
        gType :: String
    } deriving Show 

data Keyboard = Keyboard
    deriving Show

--data Action = Action 
 --   deriving Show 

data VKResponse = VKResponse {
        key :: String
        , server :: String
        , currentTs :: String 
    } 
    | VKError {
        error_code :: Int 
        , error_msg :: T.Text
    } 
    | VKParseError deriving Show

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

data VKSend = VKSend {
    respPeerId :: Int
    , respMessage :: Maybe T.Text
    , respRandomId :: Int 
} deriving Show 

data VKResult = SendMsgScs {
        newTs :: Int
    } 
    | SendMsgError {
        resError :: VKResultError
    } deriving Show 

instance FromJSON VKResult where 
    parseJSON = withObject "VKResult" $ \obj -> do 
        isError <- obj .:? "error"
        case isError of 
            Just val -> pure $ SendMsgError val 
            Nothing -> SendMsgScs <$> obj .: "response"

data VKResultError = VKResultError {
    errCode :: Int
    , errMsg :: T.Text
} deriving Show 

instance FromJSON VKResultError where 
    parseJSON (Object obj) = VKResultError <$> 
        obj .: "error_code"
        <*> obj .: "error_msg"
    parseJSON _ = parseFail parseFailMessage

data VKKeyBoard = VKKeyBoard 
    {
        one_time :: Bool
        , inline :: Bool
        , buttons :: [[Action]] 
    } deriving (Show, Generic) 

instance ToJSON VKKeyBoard 

data Action = Action VKButtons 
    deriving (Show)

instance ToJSON Action where 
    toJSON (Action btns) = object ["action" .= btns]

data VKButtons = VKButtons 
    {
        butType :: String
        , payload :: VKPayload
        , label :: String 
    } deriving Show

data VKPayload = VKPayload 
    {
        plType :: T.Text 
        , plVal :: String 
    } deriving Show 

instance ToJSON VKPayload where 
    toJSON (VKPayload t v) = object [ t .= v]

instance ToJSON VKButtons where 
    toJSON (VKButtons btnType pld btnLab) = object ["type" .= btnType
        , "payload" .= pld
        , "label" .= btnLab]


