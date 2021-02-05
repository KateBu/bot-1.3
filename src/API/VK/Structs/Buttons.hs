module API.VK.Structs.Buttons where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import qualified Data.Text as T
import GHC.Generics (Generic)

data VKKeyBoard = VKKeyBoard
  { inline :: Bool,
    buttons :: [[ButtonAction]]
  }
  deriving (Show, Generic)

instance ToJSON VKKeyBoard

newtype ButtonAction = ButtonAction VKButtons
  deriving (Show)

instance ToJSON ButtonAction where
  toJSON (ButtonAction btns) = object ["action" .= btns]

data VKButtons = VKButtons
  { button_type :: T.Text,
    button_payload :: T.Text,
    button_label :: T.Text
  }
  deriving (Show)

instance ToJSON VKButtons where
  toJSON (VKButtons buttonType payload label) =
    object
      [ "type" .= buttonType,
        "payload" .= payload,
        "label" .= label
      ]
