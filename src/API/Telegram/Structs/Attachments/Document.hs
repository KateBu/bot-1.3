module API.Telegram.Structs.Attachments.Document where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

newtype Document = Document
  { document_file_id :: T.Text
  }
  deriving (Show)

instance FromJSON Document where
  parseJSON (Object v) =
    Document <$> v .: "file_id"
  parseJSON _ = parseFail parseFailMessage
