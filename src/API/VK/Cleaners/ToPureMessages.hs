module API.VK.Cleaners.ToPureMessages where

import qualified API.VK.Structs as VKStructs 
import qualified Logic.PureStructs as PureStructs
import qualified Logger.Logger as Logger
import qualified Config.Config as Config 
import API.VK.Cleaners.Params ( baseParams, makeParams ) 
import API.VK.Cleaners.MessageTypes ( getMessageType )

makePureMessage :: Config.Config 
    -> PureStructs.UpdateID
    -> VKStructs.VKMessage 
    -> Either Logger.LogMessage PureStructs.PureMessage
makePureMessage config uid vkMsg = either Left (makePureMessage' config uid vkMsg) (getMessageType vkMsg)

makePureMessage' :: Config.Config 
    -> PureStructs.UpdateID
    -> VKStructs.VKMessage 
    -> PureStructs.MessageType
    -> Either Logger.LogMessage PureStructs.PureMessage
makePureMessage' _ uid vkMsg mType@(PureStructs.MTCallbackQuery callback) = 
    Right $ PureStructs.PureMessage
    mType
    uid 
    (Just $ VKStructs.from_id vkMsg)
    (Just $ 
        (PureStructs.ParamsText "message" (PureStructs.newRepeatText $ PureStructs.getNewRep callback) 
            : baseParams vkMsg))  
makePureMessage' config uid vkMsg mType@(PureStructs.MTUserCommand _) =  
    Right $ PureStructs.PureMessage 
    mType
    uid 
    (Just $ VKStructs.from_id vkMsg)
    (makeParams config mType vkMsg)
makePureMessage' config uid vkMsg mType@(PureStructs.MTCommon _) =
    Right $ PureStructs.PureMessage 
    mType 
    uid 
    (Just $ VKStructs.from_id vkMsg)
    (makeParams config mType vkMsg)
makePureMessage' _ uid _ _ =
    Right $ PureStructs.PureMessage 
    PureStructs.MTEmpty 
    uid 
    Nothing 
    Nothing 