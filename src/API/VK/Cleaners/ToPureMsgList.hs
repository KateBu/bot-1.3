module API.VK.Cleaners.ToPureMsgList where

import qualified Data.ByteString.Lazy as BSL 
import qualified Data.Text as T 
import Data.Aeson ( eitherDecode )
import qualified API.VK.Structs as VKStructs 
import qualified Logic.PureStructs as PureStructs
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Config.Config as Config 
import API.VK.Cleaners.ToPureMessages ( makePureMessage )

vkByteStringToPureMessageList :: Config.Config -> Logger.Logger
    -> Either Logger.LogMessage BSL.ByteString 
    -> IO (Either Logger.LogMessage [PureStructs.PureMessage]) 
vkByteStringToPureMessageList config logger bs = decodeByteString logger bs >>= vkUpdInfoToPureMessageList config  logger 

vkUpdInfoToPureMessageList ::Config.Config -> Logger.Logger
    ->  Either Logger.LogMessage (PureStructs.UpdateID, [VKStructs.VKUpdInfo])
    -> IO (Either Logger.LogMessage [PureStructs.PureMessage])
vkUpdInfoToPureMessageList _ _ (Left err) = pure $ Left err 
vkUpdInfoToPureMessageList config logger (Right (uid, upds)) = do 
    Logger.botLog logger LoggerMsgs.parseVKMsgScs
    pure $ mapM (vkUpdInfoToPureMessage config uid) upds

vkUpdInfoToPureMessage ::Config.Config -> PureStructs.UpdateID -> VKStructs.VKUpdInfo
    -> Either Logger.LogMessage PureStructs.PureMessage
vkUpdInfoToPureMessage config uid updInfo = case VKStructs.updType updInfo of 
    VKStructs.OtherEvent -> Left LoggerMsgs.unexpVKEvent
    _ -> do 
        let mbUpdObj = VKStructs.updObj updInfo
        maybe (noUpdObj uid) (justUpdObj config uid) mbUpdObj 

noUpdObj :: PureStructs.UpdateID -> Either Logger.LogMessage PureStructs.PureMessage 
noUpdObj uid = Right (PureStructs.PureMessage PureStructs.MTEmpty uid Nothing Nothing) 

justUpdObj :: Config.Config -> PureStructs.UpdateID -> VKStructs.VKObject
    -> Either Logger.LogMessage PureStructs.PureMessage
justUpdObj config uid obj = do 
    let vkMessage = VKStructs.vkMessage obj  
    makePureMessage config uid vkMessage 

decodeByteString :: Logger.Logger 
    -> Either Logger.LogMessage BSL.ByteString 
    -> IO (Either Logger.LogMessage (PureStructs.UpdateID, [VKStructs.VKUpdInfo]))
decodeByteString logger = either (pure . Left) (decodeByteString' logger) 
    
decodeByteString' :: Logger.Logger 
    -> BSL.ByteString 
    -> IO (Either Logger.LogMessage (PureStructs.UpdateID, [VKStructs.VKUpdInfo]))
decodeByteString' logger json = do 
    Logger.botLog logger LoggerMsgs.getVKUpdScs
    let eiUpdates = eitherDecode json :: Either String VKStructs.VKUpdates
    either decodeUpdErr decodeUpdScs eiUpdates 

decodeUpdErr :: String 
    -> IO (Either Logger.LogMessage (PureStructs.UpdateID, [VKStructs.VKUpdInfo]))
decodeUpdErr err = pure $ 
    Left (Logger.LogMessage Logger.Error ("decode vk update failed: " <> (T.pack err)))

decodeUpdScs :: VKStructs.VKUpdates 
    -> IO (Either Logger.LogMessage (PureStructs.UpdateID, [VKStructs.VKUpdInfo]))
decodeUpdScs (VKStructs.VKUpdateError (VKStructs.UpdateErr errCode _)) = case errCode of 
    1 -> pure $ Left LoggerMsgs.vkUpdatesFailed1  
    2 -> pure $ Left LoggerMsgs.vkUpdatesFailed2
    3 -> pure $ Left LoggerMsgs.vkUpdatesFailed3
    _ -> pure $ Left LoggerMsgs.vkUpdatesFailed4 
decodeUpdScs (VKStructs.VKUpdates upd) = pure $ 
    Right (read (VKStructs.ts upd), (VKStructs.updates upd)) 