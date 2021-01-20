module API.VK.Cleaners.Params (makeParams, baseParams) where

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
import qualified API.VK.Structs as VKStructs
import qualified Config.Config as Config
import qualified Logic.PureStructs as PureStructs

makeParams ::
  Config.Config ->
  PureStructs.MessageType ->
  VKStructs.VKMessage ->
  Maybe [PureStructs.Params]
makeParams config (PureStructs.MTUserCommand PureStructs.Help) vkMsg = undefined
 {- pure $
    PureStructs.ParamsText "message" (Config.helpMessage config) : baseParams vkMsg-}
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
