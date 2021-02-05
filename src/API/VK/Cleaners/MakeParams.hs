module API.VK.Cleaners.MakeParams (makeParams, baseParams) where

import API.VK.Cleaners.Keyboard (keyboard)
import API.VK.Cleaners.Params.AttachParams
  ( attachmentListParams,
    makeAttachParams,
  )
import API.VK.Cleaners.Params.BaseParams (baseParams)
import API.VK.Cleaners.Params.Fwd (getFwdMsgIds)
import API.VK.Cleaners.Params.SharedFunctions
  ( setMaybeDoubleParam,
    setMessageParam,
  )
import qualified API.VK.Structs.Exports as VKStructs
import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs

makeParams ::
  Env.HelpMessage ->
  PureStructs.MessageType ->
  VKStructs.VKMessage ->
  Maybe [PureStructs.Params]
makeParams helpMsg (PureStructs.MsgTypeUserCommand PureStructs.Help) vkMsg =
  pure $
    PureStructs.ParamsText "message" helpMsg : baseParams vkMsg
makeParams _ (PureStructs.MsgTypeUserCommand PureStructs.Repeat) vkMsg =
  pure $
    baseParams vkMsg
      <> [ PureStructs.ParamsText "message" PureStructs.repeatText,
           PureStructs.ParamsJSON "keyboard" keyboard
         ]
makeParams _ (PureStructs.MsgTypeCommon "Message") vkMsg = do
  txt <- VKStructs.msg_text vkMsg
  pure $ PureStructs.ParamsText "message" txt : baseParams vkMsg
makeParams _ (PureStructs.MsgTypeCommon "Geo") vkMsg =
  pure $
    setMessageParam (VKStructs.msg_text vkMsg)
      <> setMaybeDoubleParam "lat" getLatitude vkMsg
      <> setMaybeDoubleParam "long" getLongitude vkMsg
      <> baseParams vkMsg
makeParams _ (PureStructs.MsgTypeCommon "Fwd") vkMsg = do
  let msgIds = getFwdMsgIds (VKStructs.fwd_msgs vkMsg)
  pure $
    setMessageParam (VKStructs.msg_text vkMsg)
      <> [PureStructs.ParamsText "forward_messages" msgIds]
      <> baseParams vkMsg
makeParams _ (PureStructs.MsgTypeCommon "Attachment") vkMsg = do
  let attachParams = attachmentListParams vkMsg (VKStructs.attachments vkMsg)
  makeAttachParams vkMsg attachParams
makeParams _ _ _ = Nothing

getLatitude :: VKStructs.VKMessage -> Maybe Double
getLatitude = fmap (VKStructs.latitude . VKStructs.geo_coordinates) . VKStructs.geo

getLongitude :: VKStructs.VKMessage -> Maybe Double
getLongitude = fmap (VKStructs.longitude . VKStructs.geo_coordinates) . VKStructs.geo
