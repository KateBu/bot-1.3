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
  ( MessageInfo (..),
    PollOptions (..),
    TelAmination (..),
    TelAudio (..),
    TelChat (..),
    TelContact (..),
    TelDocument (..),
    TelLocation (..),
    TelPhoto (..),
    TelPoll (..),
    TelSticker (..),
    TelVenue (..),
    TelVideo (..),
    TelVoice (..),
  )
import API.Telegram.Structs.UpdateErr as Structs
  ( TelegramUpdatesError (..),
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

data TelegramUpdates = TelegramUpdates
  { ok :: Bool,
    result :: [TelUpdateResult]
  }
  deriving (Generic, Show)

instance FromJSON TelegramUpdates

data TelUpdateResult = TelUpdateResult
  { update_id :: Int,
    message_info :: Maybe MessageInfo,
    callback_query :: Maybe Callback
  }
  deriving (Show)

instance FromJSON TelUpdateResult where
  parseJSON (Object v) =
    TelUpdateResult <$> v .: "update_id"
      <*> v .:? "message"
      <*> v .:? "callback_query"
  parseJSON _ = parseFail parseFailMessage
