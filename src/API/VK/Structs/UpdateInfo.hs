module API.VK.Structs.UpdateInfo
  ( module API.VK.Structs.UpdateInfo,
    module Structs,
  )
where

import API.VK.Structs.Message as Structs
  ( AObject (..),
    AccessKey,
    Attachment (..),
    Coordinates (..),
    Geo (..),
    ItemID,
    Message (..),
    OwnerID,
    Url,
  )
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    withObject,
    (.:),
    (.:?),
  )
import Data.Aeson.Types (parseFail)
import TextMessages.ParseFailMessage (parseFailMessage)

data EventType = MsgNew | OtherEvent
  deriving (Show)

data UpdateInfo = UpdateInfo
  { update_type :: EventType,
    update_object :: Maybe MessageObject
  }
  deriving (Show)

instance FromJSON UpdateInfo where
  parseJSON = withObject "UpdateInfo" $ \obj -> do
    updateType <- obj .: "type"
    case (updateType :: String) of
      "message_new" ->
        UpdateInfo MsgNew
          <$> obj .:? "object"
      _ -> pure $ UpdateInfo OtherEvent Nothing

newtype MessageObject = MessageObject
  { message :: Message
  }
  deriving (Show)

instance FromJSON MessageObject where
  parseJSON (Object obj) = MessageObject <$> obj .: "message"
  parseJSON _ = parseFail parseFailMessage
