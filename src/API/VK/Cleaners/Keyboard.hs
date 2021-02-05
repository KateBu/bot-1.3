module API.VK.Cleaners.Keyboard where

import qualified API.VK.Structs.Exports as VKStructs
import Data.Aeson (KeyValue ((.=)), Value, encode, object)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as C8 (unpack)
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

keyboard :: Value
keyboard =
  object
    [ "inline" .= True,
      "buttons" .= (fmap . fmap) pureButtonsToVKButtonAction PureStructs.buttons
    ]

pureButtonsToVKButtonAction :: PureStructs.PureButtons -> VKStructs.ButtonAction
pureButtonsToVKButtonAction btn@(PureStructs.PureButtons btnLabel _) =
  VKStructs.ButtonAction $
    VKStructs.VKButtons
      "callback"
      ((T.pack . C8.unpack) $ pureButtonsToBytestring btn)
      btnLabel

pureButtonsToBytestring :: PureStructs.PureButtons -> BSL.ByteString
pureButtonsToBytestring (PureStructs.PureButtons btnLabel btnData) = encode $ object [btnLabel .= btnData]
