module API.VK.Structs.Buttons where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import qualified Data.Text as T
import GHC.Generics (Generic)

data VKKeyBoard = VKKeyBoard
  { inline :: Bool,
    buttons :: [[BtnAction]]
  }
  deriving (Show, Generic)

instance ToJSON VKKeyBoard

newtype BtnAction = BtnAction VKButtons
  deriving (Show)

instance ToJSON BtnAction where
  toJSON (BtnAction btns) = object ["action" .= btns]

data VKButtons = VKButtons
  { butType :: T.Text,
    payload :: T.Text,
    label :: T.Text
  }
  deriving (Show)

instance ToJSON VKButtons where
  toJSON (VKButtons btnType pld btnLab) =
    object
      [ "type" .= btnType,
        "payload" .= pld,
        "label" .= btnLab
      ]
