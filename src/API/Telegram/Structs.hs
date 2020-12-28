module API.Telegram.Structs (module Structs) where

import API.Telegram.TStructs.UpdateErr as Structs
    ( TelegramUpdatesError(..) )
import API.Telegram.TStructs.Updates as Structs
    ( TelUpdateResult(..),
      TelegramUpdates(..),
      CBChat(..),
      CBMsg(..),
      Callback(..),
      MessageInfo(..),
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
import API.Telegram.TStructs.Buttons as Structs
    ( InlineKeyBoard(..), Button(..) ) 