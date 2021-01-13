module API.Telegram.TStructs.MessageInfo
  ( module API.Telegram.TStructs.MessageInfo,
    module Structs,
  )
where

import API.Messages (parseFailMessage)
import API.Telegram.TStructs.Attachments.Animation as Structs
  ( TelAmination (..),
  )
import API.Telegram.TStructs.Attachments.Audio as Structs (TelAudio (..))
import API.Telegram.TStructs.Attachments.Contact as Structs (TelContact (..))
import API.Telegram.TStructs.Attachments.Document as Structs
  ( TelDocument (..),
  )
import API.Telegram.TStructs.Attachments.Location as Structs
  ( TelLocation (..),
  )
import API.Telegram.TStructs.Attachments.Photo as Structs (TelPhoto (..))
import API.Telegram.TStructs.Attachments.Poll as Structs
  ( PollOptions (..),
    TelPoll (..),
  )
import API.Telegram.TStructs.Attachments.Sticker as Structs (TelSticker (..))
import API.Telegram.TStructs.Attachments.Venue as Structs (TelVenue (..))
import API.Telegram.TStructs.Attachments.Video as Structs (TelVideo (..))
import API.Telegram.TStructs.Attachments.Voice as Structs (TelVoice (..))
import API.Telegram.TStructs.Chat as Structs (TelChat (..))
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T

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
