module API.VK.Cleaners where

import qualified Data.Text as T 

import API.VK.Structs
import Logic.PureStructs
import Logger.Logger
import qualified Logger.LoggerMsgs as LoggerMsgs

updatesToPureMessageList :: (VKUpdates, UpdateID) -> IO (Either LogMessage [Message])
updatesToPureMessageList (upds, uid) = do 
    --let uid = read $ ts upds :: UpdateID 
    msgs <- mapM vkUpdatesToMessage (zip (updates upds) [uid ..])
    pure $ (sequence msgs) 



vkUpdatesToMessage :: (VKUpdInfo, UpdateID) -> IO (Either LogMessage Message) 
vkUpdatesToMessage ((VKUpdInfo OtherEvent _ _), uid) = pure $ Left LoggerMsgs.vkUpdatesParsingUnknownMsgType
vkUpdatesToMessage ((VKUpdInfo MsgNew msg _), uid) = case msg of 
    Nothing -> pure $ Left LoggerMsgs.vkUpdatesParsingNoMsg
    Just msg -> do 
        let chid = from_id msg
        let cMsg = updatesToComMessage msg 
        pure $ Right (CommonMessage uid chid cMsg Nothing) 


updatesToComMessage :: VKMessage -> CMessage 
updatesToComMessage msg = case msgText msg of 
    Just val -> Txt val 
    Nothing -> Other  


    