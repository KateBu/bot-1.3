module API.Telegram.Structs.Attachments.Photo where

import TextMessages.ParseFailMessage (parseFailMessage)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T

data TelPhoto = TelPhoto
  { photo_file_id :: T.Text
  }
  deriving (Show)

instance FromJSON TelPhoto where
  parseJSON (Object v) =
    TelPhoto <$> v .: "file_id"
  parseJSON _ = parseFail parseFailMessage
