module API.Telegram.Structs where

import qualified Data.Text as T 
import GHC.Generics ( Generic )
import Data.Aeson
    ( (.:),
      (.:?),
      object,
      FromJSON(parseJSON),
      Value(Object),
      KeyValue((.=)),
      ToJSON(toJSON) ) 
import Data.Aeson.Types (parseFail)


parseFailMessage :: String
parseFailMessage = "Telegram bot got unexpected imput data type while parsing JSON"

data TelegramUpdatesError = TelegramUpdatesError 
    {
        error_code:: Int
        , description :: T.Text
    } deriving (Generic, Show)

instance FromJSON TelegramUpdatesError

data TelegramUpdates = TelegramUpdates 
    {
        ok :: Bool
        , result :: [TelUpdateResult]
    } deriving (Generic, Show)

instance FromJSON TelegramUpdates

data TelUpdateResult = TelUpdateResult 
    {
        update_id :: Int
        , messageInfo ::Maybe  MessageInfo
        , callback_query :: Maybe Callback 
    } deriving Show 

instance FromJSON TelUpdateResult where 
    parseJSON (Object v) =
        TelUpdateResult <$> v .: "update_id"
        <*> v .:? "message"
        <*> v .:? "callback_query"
    parseJSON _ = parseFail parseFailMessage

data Callback = Callback 
    {
        cb_msg :: CBMsg
        , cb_data :: T.Text
    } deriving Show 

instance FromJSON Callback where 
    parseJSON (Object v) = 
        Callback <$> v .: "message"
        <*> v .: "data"
    parseJSON _ = parseFail parseFailMessage

data CBMsg = CBMsg 
    {
        cb_chat :: CBChat
    } deriving Show 

instance FromJSON CBMsg where 
    parseJSON (Object v) = 
        CBMsg <$> v .: "chat"
    parseJSON _ = parseFail parseFailMessage

data CBChat = CBChat
    {
        cb_chid :: Int 
    } deriving Show 

instance FromJSON CBChat where 
    parseJSON (Object v) = 
        CBChat <$> v .: "id"
    parseJSON _ = parseFail parseFailMessage

data MessageInfo = MessageInfo 
    {
        message_id :: Int
        , chat :: TelChat
        , txt :: Maybe T.Text
        , animation :: Maybe TelAmination
        , audio :: Maybe TelAudio
        , document :: Maybe TelDocument
        , photo :: Maybe [TelPhoto]
        , video :: Maybe TelVideo
        , voice :: Maybe TelVoice
        , contact :: Maybe TelContact
        , poll :: Maybe TelPoll
        , venue :: Maybe TelVenue
        , location :: Maybe TelLocation
        , mediagroup :: Maybe T.Text
        , caption :: Maybe T.Text
        , sticker :: Maybe TelSticker
    } deriving Show 

instance FromJSON MessageInfo where 
    parseJSON (Object v) = 
        MessageInfo <$> v .: "message_id"
        <*> v .: "chat"
        <*> v .:? "text"
        <*> v .:? "animation"
        <*> v .:? "audio"
        <*> v .:? "document"
        <*> v .:? "photo"
        <*> v .:? "video"
        <*> v .:? "voice"
        <*> v .:? "contact"
        <*> v .:? "poll"
        <*> v .:? "venue"
        <*> v .:? "location"
        <*> v .:? "media_group_id"
        <*> v .:? "caption" 
        <*> v .:? "sticker"
    parseJSON _ = parseFail parseFailMessage

data TelChat = TelChat
    {
        chat_id :: Int
        ,chat_type :: T.Text 
    } deriving Show 

instance FromJSON TelChat where 
    parseJSON (Object v) = 
        TelChat <$> v .: "id"
            <*> v .: "type"
    parseJSON _ = parseFail parseFailMessage

data TelSticker = TelSticker
    {
        s_file_id :: T.Text
        , s_is_animated :: Bool 

    } deriving Show 

instance FromJSON TelSticker where 
    parseJSON (Object v) = 
        TelSticker <$> v .: "file_id"
        <*> v .: "is_animated"
    parseJSON _ = parseFail parseFailMessage

data TelAudio = TelAudio 
    {
        audio_id :: T.Text
        , audio_duration :: Maybe Int
        , audio_performer :: Maybe T.Text
        , audio_title :: Maybe T.Text
    } deriving Show

instance FromJSON TelAudio where 
    parseJSON (Object v) = 
        TelAudio <$> v .: "file_id"
        <*> v .:? "duration"
        <*> v .:? "performer"
        <*> v .:? "title"
    parseJSON _ = parseFail parseFailMessage

data TelPhoto = TelPhoto 
    {
        photo_file_id :: T.Text
    } deriving Show

instance FromJSON TelPhoto where 
    parseJSON (Object v) = 
        TelPhoto <$> v .: "file_id"
    parseJSON _ = parseFail parseFailMessage

data TelDocument = TelDocument 
    {
         doc_file_id :: T.Text
    } deriving Show 

instance FromJSON TelDocument where 
    parseJSON (Object v) = 
        TelDocument <$> v .: "file_id"
    parseJSON _ = parseFail parseFailMessage

data TelLocation = TelLocation
    {
        latitude :: Double
        , longitude :: Double 
    } deriving (Generic, Show ) 

instance FromJSON TelLocation

data TelVideo = TelVideo 
    {
        video_file_id :: T.Text
    } deriving Show 

instance FromJSON TelVideo where 
    parseJSON (Object v) = 
        TelVideo <$> v .: "file_id"
    parseJSON _ = parseFail parseFailMessage

data TelVoice = TelVoice
    {
        voice_file_id :: T.Text
    } deriving Show 

instance FromJSON TelVoice where 
    parseJSON (Object v) = 
        TelVoice <$> v .: "file_id"
    parseJSON _ = parseFail parseFailMessage

data TelAmination = TelAmination 
    {
        animation_file_id :: T.Text
    } deriving Show 

instance FromJSON TelAmination where 
    parseJSON (Object v) =
        TelAmination <$> v .: "file_id"
    parseJSON _ = parseFail parseFailMessage

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

data TelPoll = TelPoll 
    {
        question :: T.Text 
        , poll_options :: [PollOptions]
        , is_anonymous :: Maybe Bool
        , poll_type :: Maybe T.Text
        , allows_multiple_answers :: Maybe Bool
        , correct_option_id :: Maybe Int
        , explanation :: Maybe T.Text
        , open_period :: Maybe Int 
        , close_date :: Maybe Int
        , is_closed :: Maybe Bool 
    } deriving Show 

instance FromJSON TelPoll where 
    parseJSON (Object v) = 
        TelPoll <$> v .: "question"
        <*> v .: "options"
        <*> v .:? "is_anonymous"
        <*> v .:? "type"
        <*> v .:? "allows_multiple_answers"
        <*> v .:? "correct_option_id"
        <*> v .:? "explanation"
        <*> v .:? "open_period"
        <*> v .:? "close_date"
        <*> v .:? "is_closed"
    parseJSON _ = parseFail parseFailMessage

data PollOptions = PollOptions 
    {
        poll_option :: T.Text
        , voter_count :: Int
    } deriving Show 

instance FromJSON PollOptions where 
    parseJSON (Object v) = 
        PollOptions <$> v .: "text"
        <*> v .: "voter_count"
    parseJSON _ = parseFail parseFailMessage

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

data Button = Button T.Text T.Text
    deriving Show 

instance ToJSON Button where 
    toJSON (Button btn cbd) = object ["text" .= btn, "callback_data" .= cbd] 

data InlineKeyBoard = InlineKeyBoard [[Button]]
    deriving Show 

instance ToJSON InlineKeyBoard where 
    toJSON (InlineKeyBoard btns) = object ["inline_keyboard" .= btns]

