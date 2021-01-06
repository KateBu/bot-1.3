module API.Telegram.TStructs.Updates
  ( module API.Telegram.TStructs.Updates,
    module Structs,
  )
where

import API.Messages (parseFailMessage)
import API.Telegram.TStructs.Callback as Structs
  ( CBChat (..),
    CBMsg (..),
    Callback (..),
  )
import API.Telegram.TStructs.MessageInfo as Structs
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
import API.Telegram.TStructs.UpdateErr as Structs
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

data TelegramUpdates = TelegramUpdates
  { ok :: Bool,
    result :: [TelUpdateResult]
  }
  deriving (Generic, Show)

instance FromJSON TelegramUpdates

data TelUpdateResult = TelUpdateResult
  { update_id :: Int,
    messageInfo :: Maybe MessageInfo,
    callback_query :: Maybe Callback
  }
  deriving (Show)

instance FromJSON TelUpdateResult where
  parseJSON (Object v) =
    TelUpdateResult <$> v .: "update_id"
      <*> v .:? "message"
      <*> v .:? "callback_query"
  parseJSON _ = parseFail parseFailMessage
