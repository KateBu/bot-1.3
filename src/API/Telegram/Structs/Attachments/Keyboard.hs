module API.Telegram.Structs.Attachments.Keyboard where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import qualified Logic.PureStructs as PureStructs

newtype TButtons = TButtons PureStructs.PureButtons

instance ToJSON TButtons where
  toJSON (TButtons (PureStructs.PureButtons label txt)) = object ["text" .= label, "callback_data" .= txt]
