module API.Telegram.Structs.MessageInfo
  ( module API.Telegram.Structs.MessageInfo,
    module Structs,
  )
where

import API.Telegram.Structs.Attachments.Animation as Structs
  ( Amination (..),
  )
import API.Telegram.Structs.Attachments.Audio as Structs (Audio (..))
import API.Telegram.Structs.Attachments.Contact as Structs (Contact (..))
import API.Telegram.Structs.Attachments.Document as Structs
  ( Document (..),
  )
import API.Telegram.Structs.Attachments.Location as Structs
  ( Location (..),
  )
import API.Telegram.Structs.Attachments.Photo as Structs (Photo (..))
import API.Telegram.Structs.Attachments.Poll as Structs
  ( Poll (..),
    PollOptions (..),
  )
import API.Telegram.Structs.Attachments.Sticker as Structs (Sticker (..))
import API.Telegram.Structs.Attachments.Venue as Structs (Venue (..))
import API.Telegram.Structs.Attachments.Video as Structs (Video (..))
import API.Telegram.Structs.Attachments.Voice as Structs (Voice (..))
import API.Telegram.Structs.Chat as Structs (Chat (..))
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
    chat :: Chat,
    txt :: Maybe T.Text,
    animation :: Maybe Amination,
    audio :: Maybe Audio,
    document :: Maybe Document,
    photo :: Maybe [Photo],
    video :: Maybe Video,
    voice :: Maybe Voice,
    contact :: Maybe Contact,
    poll :: Maybe Poll,
    venue :: Maybe Venue,
    location :: Maybe Location,
    mediagroup :: Maybe T.Text,
    caption :: Maybe T.Text,
    sticker :: Maybe Sticker
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
