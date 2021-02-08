module API.VK.Functions.Builders (buildPureMessage) where

import API.VK.Functions.MsgTypes (getMessageType)
import API.VK.Functions.Params (basicParams, buildParams)
import qualified API.VK.Structs.Exports as VKStructs
import qualified Environment.Exports as Env
import qualified Logic.Structs as PureStructs

buildPureMessage ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  VKStructs.VKUpdInfo ->
  Maybe PureStructs.PureMessage
buildPureMessage helpMsg updateId updateInfo = case VKStructs.update_type updateInfo of
  VKStructs.OtherEvent -> Nothing
  _ -> do
    let mbUpdateObject = VKStructs.update_object updateInfo
    maybe (buildEmptyMsg updateId) (buildMsgWithParams helpMsg updateId) mbUpdateObject

buildEmptyMsg :: PureStructs.UpdateID -> Maybe PureStructs.PureMessage
buildEmptyMsg updateId = pure $ PureStructs.PureMessage PureStructs.MsgTypeEmpty updateId Nothing Nothing

buildMsgWithParams ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  VKStructs.VKObject ->
  Maybe PureStructs.PureMessage
buildMsgWithParams helpMsg updateId object = do
  let vkMessage = VKStructs.vkMessage object
  buildMsgWithParams' helpMsg updateId vkMessage <$> getMessageType vkMessage

buildMsgWithParams' ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  VKStructs.VKMessage ->
  PureStructs.MessageType ->
  PureStructs.PureMessage
buildMsgWithParams' _ updateId vkMsg msgType@(PureStructs.MsgTypeCallbackQuery callback) =
  PureStructs.PureMessage
    msgType
    updateId
    (Just $ VKStructs.from_id vkMsg)
    msgParams
  where
    msgParams =
      Just
        ( PureStructs.ParamsText "message" (PureStructs.newRepeatText $ PureStructs.getNewRepeatNumber callback) :
          basicParams vkMsg
        )
buildMsgWithParams' helpMsg updateId vkMsg msgType@(PureStructs.MsgTypeUserCommand _) =
  PureStructs.PureMessage
    msgType
    updateId
    (Just $ VKStructs.from_id vkMsg)
    (buildParams helpMsg msgType vkMsg)
buildMsgWithParams' helpMsg updateId vkMsg msgType@(PureStructs.MsgTypeCommon _) =
  PureStructs.PureMessage
    msgType
    updateId
    (Just $ VKStructs.from_id vkMsg)
    (buildParams helpMsg msgType vkMsg)
buildMsgWithParams' _ updateId _ _ =
  PureStructs.PureMessage
    PureStructs.MsgTypeEmpty
    updateId
    Nothing
    Nothing
