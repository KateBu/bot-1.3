module API.VK.Structs.Message
  ( module API.VK.Structs.Message,
    module Structs,
  )
where

import TextMessages.ParseFailMessage (parseFailMessage)
import API.VK.Structs.Attachments as Structs
  ( AObject (..),
    AccessKey,
    Attachment (..),
    Coordinates (..),
    Geo (..),
    ItemID,
    OwnerID,
    Url,
  )
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T

data VKMessage = VKMessage
  { id :: Int,
    from_id :: Int,
    msgText :: Maybe T.Text,
    attachments :: Maybe [Attachment],
    geo :: Maybe Geo,
    cbPayload :: Maybe T.Text,
    fwdMessages :: Maybe [VKMessage]
  }
  deriving (Show)

instance FromJSON VKMessage where
  parseJSON (Object obj) =
    VKMessage <$> obj .: "id"
      <*> obj .: "from_id"
      <*> obj .:? "text"
      <*> obj .:? "attachments"
      <*> obj .:? "geo"
      <*> obj .:? "payload"
      <*> obj .:? "fwd_messages"
  parseJSON _ = parseFail parseFailMessage
