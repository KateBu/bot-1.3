module API.Telegram.TStructs.Updates (
    module API.Telegram.TStructs.Updates
    , module Structs
    ) where

import Data.Aeson
    ( (.:), (.:?), FromJSON(parseJSON), Value(Object) ) 
import GHC.Generics ( Generic )
import Data.Aeson.Types ( parseFail ) 
import API.Messages ( parseFailMessage ) 
import API.Telegram.TStructs.Callback as Structs
    ( CBChat(..), CBMsg(..), Callback(..) ) 
import API.Telegram.TStructs.MessageInfo as Structs
    ( MessageInfo(..),
      TelChat(..),
      TelSticker(..),
      TelAudio(..),
      TelPhoto(..),
      TelDocument(..),
      TelLocation(..),
      TelVideo(..),
      TelVoice(..),
      TelAmination(..),
      TelContact(..),
      PollOptions(..),
      TelPoll(..),
      TelVenue(..) )

data TelegramUpdates = TelegramUpdates 
    {
        ok :: Bool
        , result :: [TelUpdateResult]
    } deriving (Generic, Show)

instance FromJSON TelegramUpdates

data TelUpdateResult = TelUpdateResult 
    {
        update_id :: Int
        , messageInfo ::Maybe  MessageInfo
        , callback_query :: Maybe Callback 
    } deriving Show 

instance FromJSON TelUpdateResult where 
    parseJSON (Object v) =
        TelUpdateResult <$> v .: "update_id"
        <*> v .:? "message"
        <*> v .:? "callback_query"
    parseJSON _ = parseFail parseFailMessage
