module API.VK.Cleaners.Keyboard where

import qualified API.VK.Structs as VKStructs
import Data.Aeson (KeyValue ((.=)), Value, encode, object)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as C8 (unpack)
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

keyboard :: Value
keyboard =
  object
    [ "inline" .= True,
      "buttons" .= (((fmap . fmap) pureBtnToVKBtnAct PureStructs.buttons'))
    ]

pureBtnToVKBtnAct :: PureStructs.PureButtons -> VKStructs.BtnAction
pureBtnToVKBtnAct btn@(PureStructs.PureButtons btnLabel _) =
  VKStructs.BtnAction $ VKStructs.VKButtons "callback" ((T.pack . C8.unpack) $ mkLBS btn) btnLabel

mkLBS :: PureStructs.PureButtons -> BSL.ByteString
mkLBS (PureStructs.PureButtons btnLabel btnData) = encode $ object [btnLabel .= btnData]
