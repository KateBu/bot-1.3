module API.Telegram.Structs.Callback where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

data Callback = Callback CallbackMsg T.Text
  deriving (Show)

instance FromJSON Callback where
  parseJSON (Object v) =
    Callback <$> v .: "message"
      <*> v .: "data"
  parseJSON _ = parseFail parseFailMessage

newtype CallbackMsg = CallbackMsg
  { callback_chat :: CallbackChat
  }
  deriving (Show)

instance FromJSON CallbackMsg where
  parseJSON (Object v) =
    CallbackMsg <$> v .: "chat"
  parseJSON _ = parseFail parseFailMessage

newtype CallbackChat = CallbackChat
  { callback_chat_id :: Int
  }
  deriving (Show)

instance FromJSON CallbackChat where
  parseJSON (Object v) =
    CallbackChat <$> v .: "id"
  parseJSON _ = parseFail parseFailMessage
