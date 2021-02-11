module API.Telegram.Structs.Attachments.Video where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

newtype Video = Video
  { video_file_id :: T.Text
  }
  deriving (Show)

instance FromJSON Video where
  parseJSON (Object v) =
    Video <$> v .: "file_id"
  parseJSON _ = parseFail parseFailMessage
