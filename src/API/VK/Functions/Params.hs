module API.VK.Functions.Params (buildParams, basicParams) where

import API.VK.Functions.Params.AttachmentParams
  ( buildAttachmentListParams,
    buildAttachmentParams,
  )
import API.VK.Functions.Params.FwdParams (buildFwdParams)
import API.VK.Functions.Params.KeyboardParams (keyboardParams)
import API.VK.Functions.Params.MessageParams
  ( basicParams,
    buildMaybeDoubleParam,
    buildMessageParam,
  )
import qualified API.VK.Structs.Exports as VKStructs
import qualified Environment.Exports as Env
import qualified Logic.Structs as PureStructs

buildParams ::
  Env.HelpMessage ->
  PureStructs.MessageType ->
  VKStructs.VKMessage ->
  Maybe [PureStructs.Params]
buildParams helpMsg (PureStructs.MsgTypeUserCommand PureStructs.Help) vkMsg =
  pure $
    PureStructs.ParamsText "message" helpMsg : basicParams vkMsg
buildParams _ (PureStructs.MsgTypeUserCommand PureStructs.Repeat) vkMsg =
  pure $
    basicParams vkMsg
      <> [PureStructs.ParamsText "message" PureStructs.repeatText]
      <> keyboardParams
buildParams _ (PureStructs.MsgTypeCommon "Message") vkMsg = do
  txt <- VKStructs.msg_text vkMsg
  pure $ PureStructs.ParamsText "message" txt : basicParams vkMsg
buildParams _ (PureStructs.MsgTypeCommon "Geo") vkMsg =
  pure $
    buildMessageParam (VKStructs.msg_text vkMsg)
      <> buildMaybeDoubleParam "lat" getLatitude vkMsg
      <> buildMaybeDoubleParam "long" getLongitude vkMsg
      <> basicParams vkMsg
buildParams _ (PureStructs.MsgTypeCommon "Fwd") vkMsg = do
  pure $
    buildMessageParam (VKStructs.msg_text vkMsg)
      <> buildFwdParams vkMsg
      <> basicParams vkMsg
buildParams _ (PureStructs.MsgTypeCommon "Attachment") vkMsg = do
  let attachParams = buildAttachmentListParams vkMsg (VKStructs.attachments vkMsg)
  buildAttachmentParams vkMsg attachParams
buildParams _ _ _ = Nothing

getLatitude :: VKStructs.VKMessage -> Maybe Double
getLatitude = fmap (VKStructs.latitude . VKStructs.geo_coordinates) . VKStructs.geo

getLongitude :: VKStructs.VKMessage -> Maybe Double
getLongitude = fmap (VKStructs.longitude . VKStructs.geo_coordinates) . VKStructs.geo
