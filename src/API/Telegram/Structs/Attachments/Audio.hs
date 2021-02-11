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

data Audio = Audio
  { audio_id :: T.Text,
    audio_duration :: Maybe Int,
    audio_performer :: Maybe T.Text,
    audio_title :: Maybe T.Text
  }
  deriving (Show)

instance FromJSON Audio where
  parseJSON (Object v) =
    Audio <$> v .: "file_id"
      <*> v .:? "duration"
      <*> v .:? "performer"
      <*> v .:? "title"
  parseJSON _ = parseFail parseFailMessage
