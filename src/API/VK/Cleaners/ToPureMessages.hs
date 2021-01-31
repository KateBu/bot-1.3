module API.VK.Cleaners.ToPureMessages where

import API.VK.Cleaners.GetMessageType (getMessageType)
import API.VK.Cleaners.MakeParams (baseParams, makeParams)
import qualified API.VK.Structs.Internals as VKStructs
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

mkPureMessage ::
  T.Text ->
  PureStructs.UpdateID ->
  VKStructs.VKMessage ->
  PureStructs.PureMessage
mkPureMessage hMsg uid vkMsg = mkPureMessage' hMsg uid vkMsg $ getMessageType vkMsg

mkPureMessage' ::
  T.Text ->
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
mkPureMessage' hMsg uid vkMsg mType@(PureStructs.MTUserCommand _) =
  PureStructs.PureMessage
    mType
    uid
    (Just $ VKStructs.from_id vkMsg)
    (makeParams hMsg mType vkMsg)
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
