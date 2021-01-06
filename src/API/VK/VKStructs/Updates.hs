module API.VK.VKStructs.Updates
  ( module API.VK.VKStructs.Updates,
    module Structs,
  )
where

import API.VK.VKStructs.UpdateInfo as Structs
  ( AObject (..),
    AccessKey,
    Attachment (..),
    Coordinates (..),
    EventType (..),
    Geo (..),
    ItemID,
    OwnerID,
    Url,
    VKMessage (..),
    VKObject (..),
    VKUpdInfo (..),
  )
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))

data VKUpdates = VKUpdates Updates | VKUpdateError UpdateErr

data Updates = Updates
  { ts :: String,
    updates :: [VKUpdInfo]
  }
  deriving (Show)

data UpdateErr = UpdateErr
  { failed :: Int,
    curTs :: Maybe Int
  }
  deriving (Show)

instance FromJSON VKUpdates where
  parseJSON = withObject "VKUpdates" $ \obj -> do
    resp <- obj .:? "failed"
    case (resp :: Maybe Int) of
      Just val ->
        VKUpdateError
          <$> (UpdateErr val <$> obj .:? "ts")
      Nothing ->
        VKUpdates
          <$> (Updates <$> obj .: "ts" <*> obj .: "updates")
