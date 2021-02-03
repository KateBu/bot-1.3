module API.Telegram.Structs.Attachments.Document where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

newtype TelDocument = TelDocument
  { doc_file_id :: T.Text
  }
  deriving (Show)

instance FromJSON TelDocument where
  parseJSON (Object v) =
    TelDocument <$> v .: "file_id"
  parseJSON _ = parseFail parseFailMessage
