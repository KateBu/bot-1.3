module API.Telegram.Cleaners.Keyboard where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), Value, object)
import qualified Logic.PureStructs as PureStructs

newtype TButtons = TButtons PureStructs.PureButtons

instance ToJSON TButtons where
  toJSON (TButtons (PureStructs.PureButtons lbl txt)) = object ["text" .= lbl, "callback_data" .= txt]

makeKeyboard :: [[TButtons]] -> Value
makeKeyboard btns = object ["inline_keyboard" .= btns]
