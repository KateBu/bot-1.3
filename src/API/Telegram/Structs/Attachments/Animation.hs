module API.Telegram.Structs.Attachments.Animation where

import TextMessages.ParseFailMessage (parseFailMessage)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T

data TelAmination = TelAmination
  { animation_file_id :: T.Text
  }
  deriving (Show)

instance FromJSON TelAmination where
  parseJSON (Object v) =
    TelAmination <$> v .: "file_id"
  parseJSON _ = parseFail parseFailMessage
