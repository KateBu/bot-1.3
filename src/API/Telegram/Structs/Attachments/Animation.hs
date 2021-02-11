module API.Telegram.Structs.Attachments.Animation where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

newtype Amination = Amination
  { animation_file_id :: T.Text
  }
  deriving (Show)

instance FromJSON Amination where
  parseJSON (Object v) =
    Amination <$> v .: "file_id"
  parseJSON _ = parseFail parseFailMessage
