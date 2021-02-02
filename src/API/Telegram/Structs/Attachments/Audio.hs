module API.Telegram.Structs.Attachments.Audio where

import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

data TelAudio = TelAudio
  { audio_id :: T.Text,
    audio_duration :: Maybe Int,
    audio_performer :: Maybe T.Text,
    audio_title :: Maybe T.Text
  }
  deriving (Show)

instance FromJSON TelAudio where
  parseJSON (Object v) =
    TelAudio <$> v .: "file_id"
      <*> v .:? "duration"
      <*> v .:? "performer"
      <*> v .:? "title"
  parseJSON _ = parseFail parseFailMessage
