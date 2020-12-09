module API.Telegram.Parsers where

import Data.Aeson
    ( FromJSON(parseJSON),
      Value(Object),
      ToJSON(toJSON),
      (.:),
      (.:?),
      object,
      KeyValue((.=)) ) 

import qualified API.Telegram.Structs as TStructs 

instance FromJSON TStructs.TelegramUpdates

instance FromJSON TStructs.TelegramUpdatesError

instance FromJSON TStructs.TelUpdateResult where 
    parseJSON (Object v) = 
        TStructs.TelUpdateResult <$> v .: "update_id"
        <*> v .:? "message"
        <*> v .:? "callback_query"

instance FromJSON TStructs.Callback where 
    parseJSON (Object v) = 
        TStructs.Callback <$> v .: "message"
        <*> v .: "data"

instance FromJSON TStructs.CBChat where 
    parseJSON (Object v) = 
        TStructs.CBChat <$> v .: "id"

instance FromJSON TStructs.CBMsg where 
    parseJSON (Object v) = 
        TStructs.CBMsg <$> v .: "chat"

instance FromJSON TStructs.MessageInfo where 
    parseJSON (Object v) = 
        TStructs.MessageInfo <$> v .: "message_id"
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

instance FromJSON TStructs.TelChat where 
    parseJSON (Object v) = 
        TStructs.TelChat <$> v .: "id"
            <*> v .: "type"

instance FromJSON TStructs.TelAmination where 
    parseJSON (Object v) =
        TStructs.TelAmination <$> v .: "file_id"

instance FromJSON TStructs.TelAudio where 
    parseJSON (Object v) = 
        TStructs.TelAudio <$> v .: "file_id"
        <*> v .:? "duration"
        <*> v .:? "performer"
        <*> v .:? "title"

instance FromJSON TStructs.TelDocument where 
    parseJSON (Object v) = 
        TStructs.TelDocument <$> v .: "file_id"

instance FromJSON TStructs.TelPhoto where 
    parseJSON (Object v) = 
        TStructs.TelPhoto <$> v .: "file_id"

instance FromJSON TStructs.TelVideo where 
    parseJSON (Object v) = 
        TStructs.TelVideo <$> v .: "file_id"

instance FromJSON TStructs.TelVoice where 
    parseJSON (Object v) = 
        TStructs.TelVoice <$> v .: "file_id"

instance FromJSON TStructs.TelContact where
    parseJSON (Object v) = 
        TStructs.TelContact <$> v.: "phone_number"
            <*> v .: "first_name"
            <*> v .:? "last_name"
            <*> v .:? "vcard"

instance FromJSON TStructs.TelPoll where 
    parseJSON (Object v) = 
        TStructs.TelPoll <$> v .: "question"
        <*> v .: "options"
        <*> v .:? "is_anonymous"
        <*> v .:? "type"
        <*> v .:? "allows_multiple_answers"
        <*> v .:? "correct_option_id"
        <*> v .:? "explanation"
        <*> v .:? "open_period"
        <*> v .:? "close_date"
        <*> v .:? "is_closed"

instance FromJSON TStructs.PollOptions where 
    parseJSON (Object v) = 
        TStructs.PollOptions <$> v .: "text"
        <*> v .: "voter_count"

instance FromJSON TStructs.TelVenue where 
    parseJSON (Object v) = 
        TStructs.TelVenue <$> v .: "latitude"
        <*> v .: "longitude"
        <*> v .: "title"
        <*> v .: "address"

instance FromJSON TStructs.TelLocation

instance FromJSON TStructs.TelSticker where 
    parseJSON (Object v) = 
        TStructs.TelSticker <$> v .: "file_id"
        <*> v .: "is_animated"

instance ToJSON TStructs.Button where 
    toJSON (TStructs.Button btn cbd) = object ["text" .= btn, "callback_data" .= cbd] 

instance ToJSON TStructs.InlineKeyBoard where 
    toJSON (TStructs.InlineKeyBoard btns) = object ["inline_keyboard" .= btns]