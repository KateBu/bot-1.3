module API.Telegram.Structs.Buttons where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import qualified Data.Text as T

data Button = Button T.Text T.Text
  deriving (Show)

instance ToJSON Button where
  toJSON (Button btn cbd) = object ["text" .= btn, "callback_data" .= cbd]

newtype InlineKeyBoard = InlineKeyBoard [[Button]]
  deriving (Show)

instance ToJSON InlineKeyBoard where
  toJSON (InlineKeyBoard btns) = object ["inline_keyboard" .= btns]
