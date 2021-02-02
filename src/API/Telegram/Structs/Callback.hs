module API.Telegram.Structs.Callback where

import TextMessages.ParseFailMessage (parseFailMessage)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T

data Callback = Callback CBMsg T.Text
  deriving (Show)

instance FromJSON Callback where
  parseJSON (Object v) =
    Callback <$> v .: "message"
      <*> v .: "data"
  parseJSON _ = parseFail parseFailMessage

data CBMsg = CBMsg
  { cb_chat :: CBChat
  }
  deriving (Show)

instance FromJSON CBMsg where
  parseJSON (Object v) =
    CBMsg <$> v .: "chat"
  parseJSON _ = parseFail parseFailMessage

data CBChat = CBChat
  { cb_chid :: Int
  }
  deriving (Show)

instance FromJSON CBChat where
  parseJSON (Object v) =
    CBChat <$> v .: "id"
  parseJSON _ = parseFail parseFailMessage
