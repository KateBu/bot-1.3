module API.VK.Functions.Params.Forward (buildFwdParams) where

import qualified API.PureStructs.Exports as PureStructs
import qualified API.VK.Structs.Exports as VK
import qualified Data.Text as T

buildFwdParams :: VK.Message -> [PureStructs.Params]
buildFwdParams msgs = [PureStructs.ParamsText "forward_messages" msgIds]
  where
    msgIds = getFwdMsgIds (VK.fwd_msgs msgs)

getFwdMsgIds :: Maybe [VK.Message] -> T.Text
getFwdMsgIds Nothing = ""
getFwdMsgIds (Just []) = ""
getFwdMsgIds (Just [msg]) = T.pack . show $ VK.id msg
getFwdMsgIds (Just (msg : msgs)) =
  (T.pack . show $ VK.id msg) <> ","
    <> getFwdMsgIds (Just msgs)
