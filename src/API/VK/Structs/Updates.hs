module API.VK.Structs.Updates
  ( module API.VK.Structs.Updates,
    module Structs,
  )
where

import API.VK.Structs.UpdateInfo as Structs
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
    err_current_ts :: Maybe Int
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
