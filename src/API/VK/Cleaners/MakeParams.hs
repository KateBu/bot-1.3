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
makeParams hMsg (PureStructs.MTUserCommand PureStructs.Help) vkMsg =
  pure $
    PureStructs.ParamsText "message" hMsg : baseParams vkMsg
makeParams _ (PureStructs.MTUserCommand PureStructs.Repeat) vkMsg =
  pure $
    baseParams vkMsg
      <> [ PureStructs.ParamsText "message" PureStructs.repeatText,
           PureStructs.ParamsJSON "keyboard" keyboard
         ]
makeParams _ (PureStructs.MTCommon "Message") vkMsg = do
  txt <- VKStructs.msgText vkMsg
  pure $ PureStructs.ParamsText "message" txt : baseParams vkMsg
makeParams _ (PureStructs.MTCommon "Geo") vkMsg =
  pure $
    setMessageParam (VKStructs.msgText vkMsg)
      <> setMaybeDoubleParam "lat" (fmap (VKStructs.latitude . VKStructs.gCoordinates) . VKStructs.geo) vkMsg
      <> setMaybeDoubleParam "long" (fmap (VKStructs.longitude . VKStructs.gCoordinates) . VKStructs.geo) vkMsg
      <> baseParams vkMsg
makeParams _ (PureStructs.MTCommon "Fwd") vkMsg = do
  let msgIds = getFwdMsgIds (VKStructs.fwdMessages vkMsg)
  pure $
    setMessageParam (VKStructs.msgText vkMsg)
      <> [PureStructs.ParamsText "forward_messages" msgIds]
      <> baseParams vkMsg
makeParams _ (PureStructs.MTCommon "Attachment") vkMsg = do
  let attachParams = attachmentListParams vkMsg (VKStructs.attachments vkMsg)
  makeAttachParams vkMsg attachParams
makeParams _ _ _ = Nothing
