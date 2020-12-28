module API.VK.VKStructs.Updates (
    module API.VK.VKStructs.Updates 
    , module Structs
) where

import Data.Aeson ( FromJSON(parseJSON), (.:), (.:?), withObject )
import API.VK.VKStructs.UpdateInfo as Structs
    ( Coordinates(..),
      Geo(..),
      AObject(..),
      Url,
      AccessKey,
      OwnerID,
      ItemID,
      Attachment(..),
      VKMessage(..),
      EventType(..),
      VKObject(..),
      VKUpdInfo(..) ) 

data VKUpdates = VKUpdates Updates | VKUpdateError UpdateErr 

data Updates = Updates 
    {
        ts :: String 
        , updates :: [VKUpdInfo]
    } deriving (Show) 

data UpdateErr = UpdateErr 
    {
        failed :: Int
        , curTs :: Maybe Int
    } deriving (Show)


instance FromJSON VKUpdates where 
    parseJSON = withObject "VKUpdates" $ \obj -> do
        resp <- obj .:? "failed"
        case (resp :: Maybe Int) of 
            Just val -> VKUpdateError <$>
                (UpdateErr val <$> obj .:? "ts")
            Nothing -> VKUpdates <$> 
                (Updates <$> obj .: "ts" <*> obj .: "updates") 
