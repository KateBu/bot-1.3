module API.VK.Cleaners.Params.Fwd where

import qualified API.VK.Structs as VKStructs
import qualified Data.Text as T

getFwdMsgIds :: Maybe [VKStructs.VKMessage] -> T.Text
getFwdMsgIds Nothing = ""
getFwdMsgIds (Just []) = ""
getFwdMsgIds (Just [x]) = T.pack . show $ VKStructs.id x
getFwdMsgIds (Just (x : xs)) =
  (T.pack . show $ VKStructs.id x) <> ","
    <> (getFwdMsgIds (Just xs))
