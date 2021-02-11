module API.Telegram.Structs.Attachments.Sticker where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

newtype Sticker = Sticker
  { sticker_file_id :: T.Text
  }
  deriving (Show)

instance FromJSON Sticker where
  parseJSON (Object v) =
    Sticker <$> v .: "file_id"
  parseJSON _ = parseFail parseFailMessage
