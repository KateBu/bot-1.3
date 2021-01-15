module API.Telegram.TStructs.Attachments.Video where

import API.Messages (parseFailMessage)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T

data TelVideo = TelVideo
  { video_file_id :: T.Text
  }
  deriving (Show)

instance FromJSON TelVideo where
  parseJSON (Object v) =
    TelVideo <$> v .: "file_id"
  parseJSON _ = parseFail parseFailMessage