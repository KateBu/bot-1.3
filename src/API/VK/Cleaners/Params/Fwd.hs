module API.VK.Cleaners.Params.Fwd where

import qualified API.VK.Structs.Exports as VKStructs
import qualified Data.Text as T

getFwdMsgIds :: Maybe [VKStructs.VKMessage] -> T.Text
getFwdMsgIds Nothing = ""
getFwdMsgIds (Just []) = ""
getFwdMsgIds (Just [msg]) = T.pack . show $ VKStructs.id msg
getFwdMsgIds (Just (msg : msgs)) =
  (T.pack . show $ VKStructs.id msg) <> ","
    <> getFwdMsgIds (Just msgs)
