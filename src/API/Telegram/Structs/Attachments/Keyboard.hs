module API.Telegram.Structs.Attachments.Keyboard where

import qualified API.PureStructs.Exports as PureStructs
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)

newtype Buttons = Buttons PureStructs.PureButtons

instance ToJSON Buttons where
  toJSON (Buttons (PureStructs.PureButtons label txt)) = object ["text" .= label, "callback_data" .= txt]
