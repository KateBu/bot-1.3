module API.VK.Functions.Params.FwdParams where

import qualified API.VK.Structs.Exports as VKStructs
import qualified Data.Text as T
import qualified Logic.Structs as PureStructs

buildFwdParams :: VKStructs.VKMessage -> [PureStructs.Params]
buildFwdParams msgs = [PureStructs.ParamsText "forward_messages" msgIds]
  where
    msgIds = getFwdMsgIds (VKStructs.fwd_msgs msgs)

getFwdMsgIds :: Maybe [VKStructs.VKMessage] -> T.Text
getFwdMsgIds Nothing = ""
getFwdMsgIds (Just []) = ""
getFwdMsgIds (Just [msg]) = T.pack . show $ VKStructs.id msg
getFwdMsgIds (Just (msg : msgs)) =
  (T.pack . show $ VKStructs.id msg) <> ","
    <> getFwdMsgIds (Just msgs)
