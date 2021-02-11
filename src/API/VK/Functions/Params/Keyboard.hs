module API.VK.Functions.Params.Keyboard (keyboardParams) where

import qualified API.PureStructs.Exports as PureStructs
import qualified API.VK.Structs.Exports as VK
import Data.Aeson (KeyValue ((.=)), Value, encode, object)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as C8 (unpack)
import qualified Data.Text as T

keyboardParams :: [PureStructs.Params]
keyboardParams = [PureStructs.ParamsJSON "keyboard" keyboard]

keyboard :: Value
keyboard =
  object
    [ "inline" .= True,
      "buttons" .= (fmap . fmap) buildButtonAction PureStructs.buttons
    ]

buildButtonAction :: PureStructs.PureButtons -> VK.ButtonAction
buildButtonAction btn@(PureStructs.PureButtons btnLabel _) =
  VK.ButtonAction $
    VK.Buttons
      "callback"
      ((T.pack . C8.unpack) $ encodeButtons btn)
      btnLabel

encodeButtons :: PureStructs.PureButtons -> BSL.ByteString
encodeButtons (PureStructs.PureButtons btnLabel btnData) = encode $ object [btnLabel .= btnData]
