module API.Telegram.Structs.Attachments.Photo where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

newtype Photo = Photo
  { photo_file_id :: T.Text
  }
  deriving (Show)

instance FromJSON Photo where
  parseJSON (Object v) =
    Photo <$> v .: "file_id"
  parseJSON _ = parseFail parseFailMessage
