module API.VK.Cleaners.ToPureMessages where

import API.VK.Cleaners.MessageTypes (getMessageType)
import API.VK.Cleaners.Params (baseParams, makeParams)
import qualified API.VK.Structs as VKStructs
import qualified Config.Config as Config
import qualified Logic.PureStructs as PureStructs

mkPureMessage ::
  Config.Config ->
  PureStructs.UpdateID ->
  VKStructs.VKMessage ->
  PureStructs.PureMessage
mkPureMessage config uid vkMsg = mkPureMessage' config uid vkMsg $ getMessageType vkMsg

mkPureMessage' ::
  Config.Config ->
  PureStructs.UpdateID ->
  VKStructs.VKMessage ->
  PureStructs.MessageType ->
  PureStructs.PureMessage
mkPureMessage' _ uid vkMsg mType@(PureStructs.MTCallbackQuery callback) =
  PureStructs.PureMessage
    mType
    uid
    (Just $ VKStructs.from_id vkMsg)
    ( Just $
        ( PureStructs.ParamsText "message" (PureStructs.newRepeatText $ PureStructs.getNewRep callback) :
          baseParams vkMsg
        )
    )
mkPureMessage' config uid vkMsg mType@(PureStructs.MTUserCommand _) =
  PureStructs.PureMessage
    mType
    uid
    (Just $ VKStructs.from_id vkMsg)
    (makeParams config mType vkMsg)
mkPureMessage' config uid vkMsg mType@(PureStructs.MTCommon _) =
  PureStructs.PureMessage
    mType
    uid
    (Just $ VKStructs.from_id vkMsg)
    (makeParams config mType vkMsg)
mkPureMessage' _ uid _ _ =
  PureStructs.PureMessage
    PureStructs.MTEmpty
    uid
    Nothing
    Nothing
