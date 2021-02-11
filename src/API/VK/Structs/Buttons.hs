module API.VK.Structs.Buttons where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import qualified Data.Text as T
import GHC.Generics (Generic)

data Keyboard = Keyboard
  { inline :: Bool,
    buttons :: [[ButtonAction]]
  }
  deriving (Show, Generic)

instance ToJSON Keyboard

newtype ButtonAction = ButtonAction Buttons
  deriving (Show)

instance ToJSON ButtonAction where
  toJSON (ButtonAction btns) = object ["action" .= btns]

data Buttons = Buttons
  { button_type :: T.Text,
    button_payload :: T.Text,
    button_label :: T.Text
  }
  deriving (Show)

instance ToJSON Buttons where
  toJSON (Buttons buttonType payload label) =
    object
      [ "type" .= buttonType,
        "payload" .= payload,
        "label" .= label
      ]
