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
parseFailMessage = "VK bot got unexpected input data type while parsing JSON"

data VKUpdates = VKUpdates {
        ts :: String 
        , updates :: [VKUpdInfo]
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
                obj .: "ts"
                <*> obj .: "updates" 

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
        id :: Int
 --        peer_id :: Int --reciever id 
        , from_id :: Int --sender id
        , msgText :: Maybe T.Text
        , ref :: Maybe T.Text 
        , refSource :: Maybe T.Text
        , attachments :: Maybe [Attachment]
     --   , important :: Maybe Bool
        , geo :: Maybe Geo
        , cbPayload :: Maybe T.Text  
     --   , keyboard :: Maybe Keyboard 
        , fwdMessages :: Maybe [VKMessage]
     {-   , reply_message :: Maybe VKMessage
        , action :: Maybe Action 
        , admin_author_id ::Maybe Integer -}
    --    , conversation_message_id :: Integer
     {-}   , is_cropped :: Maybe Bool
        , members_count :: Maybe Integer 
        , update_time :: Maybe Integer 
        , was_listened :: Maybe Bool 
        , pinned_at :: Maybe Bool -} 
    } deriving Show 

instance FromJSON VKMessage where 
    parseJSON (Object obj) = 
        VKMessage <$> obj .: "id"
            <*> obj .: "from_id"
            <*> obj .:? "text"
            <*> obj .:? "ref"
            <*> obj .:? "ref_source"
            <*> obj .:? "attachments"
            <*> obj .:? "geo"
            <*> obj .:? "payload"
            <*> obj .:? "fwd_messages"
    parseJSON _ = parseFail parseFailMessage

data Attachment = Attachment 
    {
        aType :: T.Text 
        , aObject :: AObject
    } deriving Show 

instance FromJSON Attachment where 
    parseJSON = withObject "Attachment" $ \obj -> do 
        attachType <- obj .: "type"
        case (attachType :: T.Text) of 
            "link" -> do 
                link <- obj .: "link"
                Attachment attachType <$> (VKLink <$> link .: "url")
            "sticker" -> do 
                sticker <- obj .: "sticker"
                Attachment attachType <$> (VKSticker <$> sticker .: "sticker_id")
            "audio" -> do 
                audio <- obj .: "audio"
                Attachment attachType <$> (VKAudio <$> audio .: "id"
                    <*> audio .: "owner_id")
            "video" -> do 
                video <- obj .: "video"
                Attachment attachType <$> (VKVideo <$> video .: "id"
                    <*> video .: "owner_id"
                    <*> video .: "access_key")
            "wall" -> do 
                wall <- obj .: "wall"
                Attachment attachType <$> (VKWall <$> wall .: "id" 
                    <*> wall .: "to_id")
            "market" -> do 
                market <- obj .: "market"
                Attachment attachType <$> (VKMarket <$> market .: "id"
                    <*> market .: "owner_id")
            "poll" -> do 
                poll <- obj .: "poll"
                Attachment attachType <$> (VKPoll <$> poll .: "id"
                    <*> poll .: "owner_id")
            "photo" -> do
                photo <- obj .: "photo" 
                Attachment attachType <$> 
                    (VKPhoto <$> photo .: "id"
                    <*> photo .: "owner_id"
                    <*> photo .: "access_key"
                    <*> photo .: "sizes")
            _ -> pure $ Attachment attachType VKUnknown

type ItemID = Int 
type OwnerID = Int 
type AccessKey = T.Text
type Url = T.Text

data AObject = 
    VKLink Url
    | VKSticker ItemID
    | VKAudio ItemID OwnerID         
    | VKVideo ItemID OwnerID AccessKey        
    | VKWall ItemID OwnerID 
    | VKMarket ItemID OwnerID 
    | VKPoll ItemID OwnerID 
    | VKPhoto ItemID OwnerID AccessKey [PhSizes]
    | VKUnknown 
    deriving Show 
--        {
  --          photoId :: Int
           -- , albumId :: Integer 
    --        , photoOwnerId :: Int 
           -- , userId :: Integer 
           -- , phText :: T.Text
      --      , photoAccessKey :: T.Text
            --, phDate :: Integer 
           -- , phSizes :: [PhSizes]
           -- , phWidth :: Integer 
           -- , phHeight :: Integer 
 --       } deriving Show 


data PhSizes = PhSizes
    {
        phUrl :: T.Text 
        , sType :: T.Text 
    } deriving (Show, Eq )

instance FromJSON PhSizes where 
    parseJSON (Object obj) = PhSizes <$> obj .: "url"
        <*> obj .: "type"

data Geo = Geo 
    {
        gCoordinates :: Coordinates
    } deriving Show 

instance FromJSON Geo where 
    parseJSON (Object obj) = Geo <$> 
        obj .: "coordinates"
    parseJSON _ = parseFail parseFailMessage

data Coordinates = Coordinates
    {
        latitude :: Double 
        , longitude :: Double 
    } deriving (Show, Generic)

instance FromJSON Coordinates 

data Keyboard = Keyboard
    deriving Show

data VKResponse = VKResponse {
        key :: T.Text
        , server :: T.Text
        , currentTs :: String 
    } 
    | VKError {
        errorCode :: Int 
        , errorMsg :: T.Text
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
        inline :: Bool
        , buttons :: [[BtnAction]] 
    } deriving (Show, Generic) 

instance ToJSON VKKeyBoard 

data BtnAction = BtnAction VKButtons 
    deriving (Show)

instance ToJSON BtnAction where 
    toJSON (BtnAction btns) = object ["action" .= btns]

data VKButtons = VKButtons 
    {
        butType :: T.Text
        , payload :: T.Text 
        , label :: T.Text 
    } deriving Show

instance ToJSON VKButtons where 
    toJSON (VKButtons btnType pld btnLab) = object ["type" .= btnType
        , "payload" .= pld
        , "label" .= btnLab]

data VKUploadPhotoServerResponse = PhotoServer 
    {
        response :: UploadUrl 
    } 
    | PhotoServerError 
    {
        phsError :: VKResultError 
    }
    deriving Show 

instance FromJSON VKUploadPhotoServerResponse where 
    parseJSON = withObject "VKUploadPhotoServerResponse" $ \obj -> do 
        isError <- obj .:? "error"
        case isError of 
            Just val -> pure $ PhotoServerError val 
            Nothing -> PhotoServer <$> obj .: "response"

data UploadUrl = UploadUrl Url  
    deriving Show 

instance FromJSON UploadUrl where 
    parseJSON (Object obj) = UploadUrl <$> obj .: "upload_url"
    parseJSON _ = parseFail parseFailMessage

data UploadPhotoResponse = UploadPhotoResponse 
    {
        photoServer :: Int 
        , photo :: T.Text
        , hash :: T.Text 
    } deriving Show 

instance FromJSON UploadPhotoResponse where 
    parseJSON (Object obj) = UploadPhotoResponse <$> obj .: "server"
        <*> obj .: "photo" 
        <*> obj .: "hash"