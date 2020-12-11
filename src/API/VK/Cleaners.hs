module API.VK.Cleaners where

import qualified API.VK.Structs as VKStructs 
import qualified Logic.PureStructs as PureStructs
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs

updatesToPureMessageList :: (VKStructs.VKUpdates, PureStructs.UpdateID) -> IO (Either Logger.LogMessage [PureStructs.Message])
updatesToPureMessageList (upds, uid) = do 
    msgs <- mapM vkUpdatesToMessage (zip (VKStructs.updates upds) [uid ..])
    pure $ (sequence msgs) 

vkUpdatesToMessage :: (VKStructs.VKUpdInfo, PureStructs.UpdateID) -> IO (Either Logger.LogMessage PureStructs.Message) 
vkUpdatesToMessage ((VKStructs.VKUpdInfo VKStructs.OtherEvent _ _), _) = pure $ Left LoggerMsgs.vkUpdatesParsingUnknownMsgType
vkUpdatesToMessage ((VKStructs.VKUpdInfo VKStructs.MsgNew msg _), uid) = case msg of 
    Nothing -> pure $ Left LoggerMsgs.vkUpdatesParsingNoMsg
    Just val -> do 
        let chid = (VKStructs.from_id . VKStructs.vkMessage) val
        pure $ updatesToMessage (VKStructs.vkMessage val) uid chid 
--        let cMsg = (updatesToComMessage . VKStructs.vkMessage) val 
--        pure $ Right (PureStructs.CommonMessage uid chid cMsg Nothing) 

updatesToComMessage :: VKStructs.VKMessage -> PureStructs.ComMessage 
updatesToComMessage msg = case VKStructs.msgText msg of 
    Just val -> PureStructs.defaultComMsg {
            PureStructs.commonMsgType = "Message"
            , PureStructs.mbText = Just val
        }
    Nothing -> PureStructs.defaultComMsg 
        {
            PureStructs.commonMsgType = "Other"
        }  

updatesToMessage :: VKStructs.VKMessage -> PureStructs.UpdateID -> PureStructs.ChatID -> Either Logger.LogMessage PureStructs.Message
updatesToMessage msg uid chid = case VKStructs.msgText msg of 
    Just txt -> case txt of 
        "/help" -> pure $ PureStructs.UserCommand uid (PureStructs.Command chid txt)
        "/repeat" -> pure $ PureStructs.UserCommand uid (PureStructs.Command chid txt)
        _ -> pure $ PureStructs.CommonMessage uid chid (updatesToComMessage msg) Nothing 
    _ -> Left LoggerMsgs.vkUpdToMsgFld 