module API.Telegram.Structs.Attachments.Sticker where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

data TelSticker = TelSticker
  { s_file_id :: T.Text,
    s_is_animated :: Bool
  }
  deriving (Show)

instance FromJSON TelSticker where
  parseJSON (Object v) =
    TelSticker <$> v .: "file_id"
      <*> v .: "is_animated"
  parseJSON _ = parseFail parseFailMessage
