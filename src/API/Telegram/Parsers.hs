module API.Telegram.Parsers where

import Data.Aeson 

import API.Telegram.Structs

instance FromJSON TelegramUpdates

instance FromJSON TelegramUpdatesError

instance FromJSON TelUpdateResult where 
    parseJSON (Object v) = 
        TelUpdateResult <$> v .: "update_id"
        <*> v .:? "message"
        <*> v .:? "callback_query"

instance FromJSON Callback where 
    parseJSON (Object v) = 
        Callback <$> v .: "message"
        <*> v .: "data"

instance FromJSON CBChat where 
    parseJSON (Object v) = 
        CBChat <$> v .: "id"

instance FromJSON CBMsg where 
    parseJSON (Object v) = 
        CBMsg <$> v .: "chat"

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

instance FromJSON TelChat where 
    parseJSON (Object v) = 
        TelChat <$> v .: "id"
            <*> v .: "type"

instance FromJSON TelAmination where 
    parseJSON (Object v) =
        TelAmination <$> v .: "file_id"

instance FromJSON TelAudio where 
    parseJSON (Object v) = 
        TelAudio <$> v .: "file_id"
        <*> v .:? "duration"
        <*> v .:? "performer"
        <*> v .:? "title"

instance FromJSON TelDocument where 
    parseJSON (Object v) = 
        TelDocument <$> v .: "file_id"

instance FromJSON TelPhoto where 
    parseJSON (Object v) = 
        TelPhoto <$> v .: "file_id"

instance FromJSON TelVideo where 
    parseJSON (Object v) = 
        TelVideo <$> v .: "file_id"

instance FromJSON TelVoice where 
    parseJSON (Object v) = 
        TelVoice <$> v .: "file_id"

instance FromJSON TelContact where
    parseJSON (Object v) = 
        TelContact <$> v.: "phone_number"
            <*> v .: "first_name"
            <*> v .:? "last_name"
            <*> v .:? "vcard"

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

instance FromJSON PollOptions where 
    parseJSON (Object v) = 
        PollOptions <$> v .: "text"
        <*> v .: "voter_count"

instance FromJSON TelVenue where 
    parseJSON (Object v) = 
        TelVenue <$> v .: "latitude"
        <*> v .: "longitude"
        <*> v .: "title"
        <*> v .: "address"

instance FromJSON TelLocation

instance FromJSON TelSticker where 
    parseJSON (Object v) = 
        TelSticker <$> v .: "file_id"
        <*> v .: "is_animated"

instance ToJSON Button where 
    toJSON (Button btn cbd) = object ["text" .= btn, "callback_data" .= cbd] 

instance ToJSON InlineKeyBoard where 
    toJSON (InlineKeyBoard btns) = object ["inline_keyboard" .= btns]