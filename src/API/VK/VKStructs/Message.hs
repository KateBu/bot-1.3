module API.VK.VKStructs.Message (
    module API.VK.VKStructs.Message
    , module Structs 
    ) where

import qualified Data.Text as T 
import Data.Aeson
    ( FromJSON(parseJSON), (.:), (.:?), Value(Object) )
import Data.Aeson.Types ( parseFail ) 
import API.Messages ( parseFailMessage )
import API.VK.VKStructs.Attachments as Structs
    ( Coordinates(..),
      Geo(..),
      AObject(..),
      Url,
      AccessKey,
      OwnerID,
      ItemID,
      Attachment(..) ) 

data VKMessage = VKMessage 
    {
        id :: Int
        , from_id :: Int
        , msgText :: Maybe T.Text
        , attachments :: Maybe [Attachment]
        , geo :: Maybe Geo
        , cbPayload :: Maybe T.Text  
        , fwdMessages :: Maybe [VKMessage]
    } deriving Show 

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
