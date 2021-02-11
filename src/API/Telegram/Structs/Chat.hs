module API.Telegram.Structs.Chat where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

data Chat = Chat
  { chat_id :: Int,
    chat_type :: T.Text
  }
  deriving (Show)

instance FromJSON Chat where
  parseJSON (Object v) =
    Chat <$> v .: "id"
      <*> v .: "type"
  parseJSON _ = parseFail parseFailMessage
