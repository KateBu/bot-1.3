module API.Telegram.Structs.Attachments.Contact where

import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

data Contact = Contact
  { phone_number :: T.Text,
    first_name :: T.Text,
    last_name :: Maybe T.Text,
    vcard :: Maybe T.Text
  }
  deriving (Show)

instance FromJSON Contact where
  parseJSON (Object v) =
    Contact <$> v .: "phone_number"
      <*> v .: "first_name"
      <*> v .:? "last_name"
      <*> v .:? "vcard"
  parseJSON _ = parseFail parseFailMessage
