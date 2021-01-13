module API.Telegram.TStructs.Attachments.Document where

import API.Messages (parseFailMessage)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T

data TelDocument = TelDocument
  { doc_file_id :: T.Text
  }
  deriving (Show)

instance FromJSON TelDocument where
  parseJSON (Object v) =
    TelDocument <$> v .: "file_id"
  parseJSON _ = parseFail parseFailMessage
