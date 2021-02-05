module API.VK.Cleaners.ToPureMessages where

import API.VK.Cleaners.GetMessageType (getMessageType)
import API.VK.Cleaners.MakeParams (baseParams, makeParams)
import qualified API.VK.Structs.Exports as VKStructs
import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs

makePureMessage ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  VKStructs.VKMessage ->
  Maybe PureStructs.PureMessage
makePureMessage helpMsg updateId vkMsg = makePureMessage' helpMsg updateId vkMsg <$> getMessageType vkMsg

makePureMessage' ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  VKStructs.VKMessage ->
  PureStructs.MessageType ->
  PureStructs.PureMessage
makePureMessage' _ updateId vkMsg msgType@(PureStructs.MsgTypeCallbackQuery callback) =
  PureStructs.PureMessage
    msgType
    updateId
    (Just $ VKStructs.from_id vkMsg)
    msgParams
  where
    msgParams =
      Just
        ( PureStructs.ParamsText "message" (PureStructs.newRepeatText $ PureStructs.getNewRepeatNumber callback) :
          baseParams vkMsg
        )
makePureMessage' helpMsg updateId vkMsg msgType@(PureStructs.MsgTypeUserCommand _) =
  PureStructs.PureMessage
    msgType
    updateId
    (Just $ VKStructs.from_id vkMsg)
    (makeParams helpMsg msgType vkMsg)
makePureMessage' helpMsg updateId vkMsg msgType@(PureStructs.MsgTypeCommon _) =
  PureStructs.PureMessage
    msgType
    updateId
    (Just $ VKStructs.from_id vkMsg)
    (makeParams helpMsg msgType vkMsg)
makePureMessage' _ updateId _ _ =
  PureStructs.PureMessage
    PureStructs.MsgTypeEmpty
    updateId
    Nothing
    Nothing
