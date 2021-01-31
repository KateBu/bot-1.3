module API.Telegram.Structs.Attachments.Voice where

import API.Messages (parseFailMessage)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T

data TelVoice = TelVoice
  { voice_file_id :: T.Text
  }
  deriving (Show)

instance FromJSON TelVoice where
  parseJSON (Object v) =
    TelVoice <$> v .: "file_id"
  parseJSON _ = parseFail parseFailMessage
