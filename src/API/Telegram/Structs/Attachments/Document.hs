module API.Telegram.Structs.Attachments.Document where

import TextMessages.ParseFailMessage (parseFailMessage)
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
