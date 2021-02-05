module API.Telegram.Structs.Buttons where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import qualified Data.Text as T

data Button = Button T.Text T.Text
  deriving (Show)

instance ToJSON Button where
  toJSON (Button button callbackData) = object ["text" .= button, "callback_data" .= callbackData]

newtype InlineKeyBoard = InlineKeyBoard [[Button]]
  deriving (Show)

instance ToJSON InlineKeyBoard where
  toJSON (InlineKeyBoard buttons) = object ["inline_keyboard" .= buttons]
