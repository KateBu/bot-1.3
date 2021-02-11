module API.VK.Functions.Params (buildParams, basicParams) where

import API.VK.Functions.Params.Attachment
  ( buildAttachmentListParams,
    buildAttachmentParams,
  )
import API.VK.Functions.Params.Fwd (buildFwdParams)
import API.VK.Functions.Params.Keyboard (keyboardParams)
import API.VK.Functions.Params.Message
  ( basicParams,
    buildMaybeDoubleParam,
    buildMessageParam,
  )
import qualified API.VK.Structs.Exports as VK
import qualified Environment.Exports as Env
import qualified Logic.Structs as PureStructs

buildParams ::
  Env.HelpMessage ->
  PureStructs.MessageType ->
  VK.Message ->
  Maybe [PureStructs.Params]
buildParams helpMsg (PureStructs.MsgTypeUserCommand PureStructs.Help) msg =
  pure $
    PureStructs.ParamsText "message" helpMsg : basicParams msg
buildParams _ (PureStructs.MsgTypeUserCommand PureStructs.Repeat) msg =
  pure $
    basicParams msg
      <> [PureStructs.ParamsText "message" PureStructs.repeatText]
      <> keyboardParams
buildParams _ (PureStructs.MsgTypeCommon "Message") msg = do
  txt <- VK.msg_text msg
  pure $ PureStructs.ParamsText "message" txt : basicParams msg
buildParams _ (PureStructs.MsgTypeCommon "Geo") msg =
  pure $
    buildMessageParam (VK.msg_text msg)
      <> buildMaybeDoubleParam "lat" getLatitude msg
      <> buildMaybeDoubleParam "long" getLongitude msg
      <> basicParams msg
buildParams _ (PureStructs.MsgTypeCommon "Fwd") msg = do
  pure $
    buildMessageParam (VK.msg_text msg)
      <> buildFwdParams msg
      <> basicParams msg
buildParams _ (PureStructs.MsgTypeCommon "Attachment") msg = do
  let attachParams = buildAttachmentListParams msg (VK.attachments msg)
  buildAttachmentParams msg attachParams
buildParams _ _ _ = Nothing

getLatitude :: VK.Message -> Maybe Double
getLatitude = fmap (VK.latitude . VK.geo_coordinates) . VK.geo

getLongitude :: VK.Message -> Maybe Double
getLongitude = fmap (VK.longitude . VK.geo_coordinates) . VK.geo
