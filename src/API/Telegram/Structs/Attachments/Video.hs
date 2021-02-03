module API.Telegram.Structs.Attachments.Video where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

newtype TelVideo = TelVideo
  { video_file_id :: T.Text
  }
  deriving (Show)

instance FromJSON TelVideo where
  parseJSON (Object v) =
    TelVideo <$> v .: "file_id"
  parseJSON _ = parseFail parseFailMessage
