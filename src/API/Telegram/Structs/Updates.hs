module API.Telegram.Structs.Updates
  ( module API.Telegram.Structs.Updates,
    module Structs,
  )
where

import API.Telegram.Structs.Callback as Structs
  ( Callback (..),
    CallbackChat (..),
    CallbackMsg (..),
  )
import API.Telegram.Structs.MessageInfo as Structs
  ( Amination (..),
    Audio (..),
    Chat (..),
    Contact (..),
    Document (..),
    Location (..),
    MessageInfo (..),
    Photo (..),
    Poll (..),
    PollOptions (..),
    Sticker (..),
    Venue (..),
    Video (..),
    Voice (..),
  )
import API.Telegram.Structs.UpdateErr as Structs
  ( UpdatesError (..),
  )
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (parseFail)
import GHC.Generics (Generic)
import TextMessages.ParseFailMessage (parseFailMessage)

data Updates = Updates
  { ok :: Bool,
    result :: [UpdateResult]
  }
  deriving (Generic, Show)

instance FromJSON Updates

data UpdateResult = UpdateResult
  { update_id :: Int,
    message_info :: Maybe MessageInfo,
    callback_query :: Maybe Callback
  }
  deriving (Show)

instance FromJSON UpdateResult where
  parseJSON (Object v) =
    UpdateResult <$> v .: "update_id"
      <*> v .:? "message"
      <*> v .:? "callback_query"
  parseJSON _ = parseFail parseFailMessage
