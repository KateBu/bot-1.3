module API.Telegram.Structs.Attachments.Voice where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

newtype Voice = Voice
  { voice_file_id :: T.Text
  }
  deriving (Show)

instance FromJSON Voice where
  parseJSON (Object v) =
    Voice <$> v .: "file_id"
  parseJSON _ = parseFail parseFailMessage
