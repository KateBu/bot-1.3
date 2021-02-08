module API.VK.Functions.Params.KeyboardParams (keyboardParams) where

import qualified API.VK.Structs.Exports as VKStructs
import Data.Aeson (KeyValue ((.=)), Value, encode, object)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as C8 (unpack)
import qualified Data.Text as T
import qualified Logic.Structs as PureStructs

keyboardParams :: [PureStructs.Params]
keyboardParams = [PureStructs.ParamsJSON "keyboard" keyboard]

keyboard :: Value
keyboard =
  object
    [ "inline" .= True,
      "buttons" .= (fmap . fmap) buildButtonAction PureStructs.buttons
    ]

buildButtonAction :: PureStructs.PureButtons -> VKStructs.ButtonAction
buildButtonAction btn@(PureStructs.PureButtons btnLabel _) =
  VKStructs.ButtonAction $
    VKStructs.VKButtons
      "callback"
      ((T.pack . C8.unpack) $ encodeButtons btn)
      btnLabel

encodeButtons :: PureStructs.PureButtons -> BSL.ByteString
encodeButtons (PureStructs.PureButtons btnLabel btnData) = encode $ object [btnLabel .= btnData]
