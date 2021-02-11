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
    Message (..),
    MessageObject (..),
    OwnerID,
    UpdateInfo (..),
    Url,
  )
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))

data Updates = UpdateSuccess UpdateSuccess | UpdateError UpdateErr

data UpdateSuccess = Updates
  { ts :: String,
    updates :: [UpdateInfo]
  }
  deriving (Show)

data UpdateErr = UpdateErr
  { failed :: Int,
    err_current_ts :: Maybe Int
  }
  deriving (Show)

instance FromJSON Updates where
  parseJSON = withObject "Updates" $ \obj -> do
    resp <- obj .:? "failed"
    case (resp :: Maybe Int) of
      Just val ->
        UpdateError
          <$> (UpdateErr val <$> obj .:? "ts")
      Nothing ->
        UpdateSuccess
          <$> (Updates <$> obj .: "ts" <*> obj .: "updates")
