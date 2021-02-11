module API.Telegram.Structs.Attachments.Keyboard where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import qualified Logic.Structs as PureStructs

newtype Buttons = Buttons PureStructs.PureButtons

instance ToJSON Buttons where
  toJSON (Buttons (PureStructs.PureButtons label txt)) = object ["text" .= label, "callback_data" .= txt]
