module API.VK.VKStructs.UpdateInfo
  ( module API.VK.VKStructs.UpdateInfo,
    module Structs,
  )
where

import API.Messages (parseFailMessage)
import API.VK.VKStructs.Message as Structs
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

data EventType = MsgNew | OtherEvent
  deriving (Show)

data VKUpdInfo = VKUpdInfo
  { updType :: EventType,
    updObj :: Maybe VKObject
  }
  deriving (Show)

instance FromJSON VKUpdInfo where
  parseJSON = withObject "VKUpdInfo" $ \obj -> do
    upType <- obj .: "type"
    case (upType :: String) of
      "message_new" ->
        VKUpdInfo MsgNew
          <$> obj .:? "object"
      _ -> pure $ VKUpdInfo OtherEvent Nothing

data VKObject = VKObject
  { vkMessage :: VKMessage
  }
  deriving (Show)

instance FromJSON VKObject where
  parseJSON (Object obj) = VKObject <$> obj .: "message"
  parseJSON _ = parseFail parseFailMessage
