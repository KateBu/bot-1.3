module API.Telegram.Structs.Chat where

import API.Messages (parseFailMessage)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T

data TelChat = TelChat
  { chat_id :: Int,
    chat_type :: T.Text
  }
  deriving (Show)

instance FromJSON TelChat where
  parseJSON (Object v) =
    TelChat <$> v .: "id"
      <*> v .: "type"
  parseJSON _ = parseFail parseFailMessage
