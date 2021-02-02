module API.Telegram.Structs.MessageInfo
  ( module API.Telegram.Structs.MessageInfo,
    module Structs,
  )
where

import API.Telegram.Structs.Attachments.Animation as Structs
  ( TelAmination (..),
  )
import API.Telegram.Structs.Attachments.Audio as Structs (TelAudio (..))
import API.Telegram.Structs.Attachments.Contact as Structs (TelContact (..))
import API.Telegram.Structs.Attachments.Document as Structs
  ( TelDocument (..),
  )
import API.Telegram.Structs.Attachments.Location as Structs
  ( TelLocation (..),
  )
import API.Telegram.Structs.Attachments.Photo as Structs (TelPhoto (..))
import API.Telegram.Structs.Attachments.Poll as Structs
  ( PollOptions (..),
    TelPoll (..),
  )
import API.Telegram.Structs.Attachments.Sticker as Structs (TelSticker (..))
import API.Telegram.Structs.Attachments.Venue as Structs (TelVenue (..))
import API.Telegram.Structs.Attachments.Video as Structs (TelVideo (..))
import API.Telegram.Structs.Attachments.Voice as Structs (TelVoice (..))
import API.Telegram.Structs.Chat as Structs (TelChat (..))
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

data MessageInfo = MessageInfo
  { message_id :: Int,
    chat :: TelChat,
    txt :: Maybe T.Text,
    animation :: Maybe TelAmination,
    audio :: Maybe TelAudio,
    document :: Maybe TelDocument,
    photo :: Maybe [TelPhoto],
    video :: Maybe TelVideo,
    voice :: Maybe TelVoice,
    contact :: Maybe TelContact,
    poll :: Maybe TelPoll,
    venue :: Maybe TelVenue,
    location :: Maybe TelLocation,
    mediagroup :: Maybe T.Text,
    caption :: Maybe T.Text,
    sticker :: Maybe TelSticker
  }
  deriving (Show)

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
