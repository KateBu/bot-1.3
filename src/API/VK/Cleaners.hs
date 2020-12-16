module API.VK.Cleaners where

--import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BSL 
import qualified Data.Text as T 
import Data.Aeson ( eitherDecode )

import qualified API.VK.Structs as VKStructs 
import qualified Logic.PureStructs as PureStructs
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Config.Config as Config 
--import qualified Config.Config as Config 

vkByteStringToPureMessageList :: Config.Config -> Logger.Logger
    -> Either Logger.LogMessage BSL.ByteString 
    -> IO (Either Logger.LogMessage [PureStructs.PureMessage]) 
vkByteStringToPureMessageList config logger bs = decodeByteString logger bs >>= vkUpdInfoToPureMessageList config 

vkUpdInfoToPureMessageList ::Config.Config ->  Either Logger.LogMessage (PureStructs.UpdateID, [VKStructs.VKUpdInfo])
    -> IO (Either Logger.LogMessage [PureStructs.PureMessage])
vkUpdInfoToPureMessageList _ (Left err) = pure $ Left err 
vkUpdInfoToPureMessageList config (Right (uid, upds)) = pure $ mapM (vkUpdInfoToPureMessage config uid) upds 

vkUpdInfoToPureMessage ::Config.Config -> PureStructs.UpdateID -> VKStructs.VKUpdInfo
    -> Either Logger.LogMessage PureStructs.PureMessage
vkUpdInfoToPureMessage config uid updInfo = case VKStructs.updType updInfo of 
    VKStructs.OtherEvent -> Left LoggerMsgs.unexpVKEvent
    _ -> do 
        case VKStructs.updObj updInfo of 
            Nothing -> Right (PureStructs.PureMessage PureStructs.MTEmpty uid Nothing Nothing)
            Just obj -> do         
                let vkMessage = VKStructs.vkMessage obj  
                let mType = getMessageType vkMessage
                case mType of 
                    PureStructs.NotImplemented -> Left LoggerMsgs.vkUpdNotImplemented
                    _ -> Right (PureStructs.PureMessage 
                        mType
                        uid  
                        (Just $ VKStructs.from_id vkMessage)
                        (makeParams config vkMessage) 
                        )
 
getMessageType :: VKStructs.VKMessage -> PureStructs.MessageType 
getMessageType vkMsg = case VKStructs.msgText vkMsg of 
    Nothing -> PureStructs.NotImplemented   
    Just txt -> case txt of 
        "/help" -> PureStructs.MTUserCommand PureStructs.Help
        "/repeat"  -> PureStructs.MTUserCommand PureStructs.Repeat
        _ -> PureStructs.MTCommon "Message"

makeParams :: Config.Config -> VKStructs.VKMessage -> Maybe [PureStructs.Params]
makeParams config vkMsg = do 
    let params = [
            PureStructs.ParamsNum "user_id" (VKStructs.from_id vkMsg)
            ]
    case getMessageType vkMsg of 
        PureStructs.NotImplemented -> Nothing 
        PureStructs.MTUserCommand PureStructs.Help -> pure $ 
            (PureStructs.ParamsText "message" (Config.helpMessage config)) : params
        PureStructs.MTUserCommand PureStructs.Repeat -> pure $ 
            (PureStructs.ParamsText "message" "Repeat command is not implemented yet") : params
        PureStructs.MTCommon _ -> do 
            txt <- VKStructs.msgText vkMsg       
            pure $ (PureStructs.ParamsText "message" txt) : params 

decodeByteString :: Logger.Logger 
    -> Either Logger.LogMessage BSL.ByteString 
    -> IO (Either Logger.LogMessage (PureStructs.UpdateID, [VKStructs.VKUpdInfo]))
decodeByteString logger eiJson = case eiJson of 
    Left err -> pure $ Left err 
    Right json -> do 
        Logger.botLog logger LoggerMsgs.getVKUpdScs
        case (eitherDecode json :: Either String VKStructs.VKUpdates) of 
            Left err -> pure $ Left (Logger.LogMessage Logger.Error ("decode vk update failed: " <> (T.pack err)))
            Right (VKStructs.VKUpdateError errCode _) -> case errCode of 
                1 -> pure $ Left LoggerMsgs.vkUpdatesFailed1  
                2 -> pure $ Left LoggerMsgs.vkUpdatesFailed2
                3 -> pure $ Left LoggerMsgs.vkUpdatesFailed3
                _ -> pure $ Left LoggerMsgs.vkUpdatesFailed4 
            Right upd -> pure $ Right (read (VKStructs.ts upd), (VKStructs.updates upd)) 
    


-- the functions below will be removed soon 
{-
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

-}