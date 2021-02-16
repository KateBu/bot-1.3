module API.VK.Functions.Builders (buildPureMessage) where

import qualified API.PureStructs.Exports as PureStructs
import API.VK.Functions.MessageTypes (buildMessageType)
import API.VK.Functions.Params (basicParams, buildParams)
import qualified API.VK.Structs.Exports as VK
import qualified Environment.Exports as Env
import qualified TextMessages.RepeatCommandMessages as RepeatCommandMessages

buildPureMessage ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  VK.UpdateInfo ->
  Maybe PureStructs.PureMessage
buildPureMessage helpMsg updateId updateInfo = case VK.update_type updateInfo of
  VK.OtherEvent -> Nothing
  _ -> do
    let mbUpdateObject = VK.update_object updateInfo
    maybe (buildEmptyMsg updateId) (buildMsgWithParams helpMsg updateId) mbUpdateObject

buildEmptyMsg :: PureStructs.UpdateID -> Maybe PureStructs.PureMessage
buildEmptyMsg updateId = pure $ PureStructs.PureMessage PureStructs.MsgTypeEmpty updateId Nothing Nothing

buildMsgWithParams ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  VK.MessageObject ->
  Maybe PureStructs.PureMessage
buildMsgWithParams helpMsg updateId object = do
  let message = VK.message object
  buildMsgWithParams' helpMsg updateId message <$> buildMessageType message

buildMsgWithParams' ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  VK.Message ->
  PureStructs.MessageType ->
  PureStructs.PureMessage
buildMsgWithParams' _ updateId msg msgType@(PureStructs.MsgTypeCallbackQuery callback) =
  PureStructs.PureMessage
    msgType
    updateId
    (Just $ VK.from_id msg)
    msgParams
  where
    msgParams =
      Just
        ( PureStructs.ParamsText
            "message"
            (RepeatCommandMessages.newRepeatText $ PureStructs.getNewRepeatNumber callback) :
          basicParams msg
        )
buildMsgWithParams' helpMsg updateId msg msgType@(PureStructs.MsgTypeUserCommand _) =
  PureStructs.PureMessage
    msgType
    updateId
    (Just $ VK.from_id msg)
    (buildParams helpMsg msgType msg)
buildMsgWithParams' helpMsg updateId msg msgType@(PureStructs.MsgTypeCommon _) =
  PureStructs.PureMessage
    msgType
    updateId
    (Just $ VK.from_id msg)
    (buildParams helpMsg msgType msg)
buildMsgWithParams' _ updateId _ _ =
  PureStructs.PureMessage
    PureStructs.MsgTypeEmpty
    updateId
    Nothing
    Nothing
