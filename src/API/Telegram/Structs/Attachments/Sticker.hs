module API.Telegram.Structs.Attachments.Sticker where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

newtype TelSticker = TelSticker
  { sticker_file_id :: T.Text
  }
  deriving (Show)

instance FromJSON TelSticker where
  parseJSON (Object v) =
    TelSticker <$> v .: "file_id"
  parseJSON _ = parseFail parseFailMessage
