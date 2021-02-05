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
    OwnerID,
    Url,
    VKMessage (..),
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

data VKUpdInfo = VKUpdInfo
  { update_type :: EventType,
    update_object :: Maybe VKObject
  }
  deriving (Show)

instance FromJSON VKUpdInfo where
  parseJSON = withObject "VKUpdInfo" $ \obj -> do
    updateType <- obj .: "type"
    case (updateType :: String) of
      "message_new" ->
        VKUpdInfo MsgNew
          <$> obj .:? "object"
      _ -> pure $ VKUpdInfo OtherEvent Nothing

newtype VKObject = VKObject
  { vkMessage :: VKMessage
  }
  deriving (Show)

instance FromJSON VKObject where
  parseJSON (Object obj) = VKObject <$> obj .: "message"
  parseJSON _ = parseFail parseFailMessage
