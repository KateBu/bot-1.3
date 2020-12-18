module API.VK.Cleaners where

--import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BSL 
import qualified Data.Text as T 
import Data.Aeson
    ( decode, eitherDecode, encode, object, Value, KeyValue((.=)) )
import Data.ByteString.Lazy.Char8 as C8 ( unpack )  
import Control.Applicative ( Alternative((<|>)) ) 

import qualified API.VK.Structs as VKStructs 
import qualified Logic.PureStructs as PureStructs
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Config.Config as Config 

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
                makePureMessage config uid vkMessage 
                
makePureMessage :: Config.Config 
    -> PureStructs.UpdateID
    -> VKStructs.VKMessage 
    -> Either Logger.LogMessage PureStructs.PureMessage
makePureMessage config uid vkMsg = case getMessageType vkMsg of 
    Left err -> Left err 
    Right mType -> case mType of 
        PureStructs.MTCallbackQuery callback -> Right $ PureStructs.PureMessage
            mType
            uid 
            (Just $ VKStructs.from_id vkMsg)
            (Just $ 
                (PureStructs.ParamsText "message" (PureStructs.newRepeatText $ PureStructs.getNewRep callback) 
                    : baseParams vkMsg))  
        PureStructs.MTUserCommand _ -> Right $ PureStructs.PureMessage 
            mType
            uid 
            (Just $ VKStructs.from_id vkMsg)
            (makeParams config mType vkMsg)
        PureStructs.MTCommon _ -> Right $ PureStructs.PureMessage 
            mType 
            uid 
            (Just $ VKStructs.from_id vkMsg)
            (makeParams config mType vkMsg)
        _ -> Left $ LoggerMsgs.notImplemented

getMessageType :: VKStructs.VKMessage -> Either Logger.LogMessage PureStructs.MessageType
getMessageType vkMsg = case msgType of 
    Nothing -> Left LoggerMsgs.notImplemented 
    Just mType -> Right mType 
    where msgType = isCallBackMsg vkMsg
            <|> isUserCommand vkMsg
            <|> isAttachmentMsg vkMsg
            <|> isTextMsg vkMsg 
    
isCallBackMsg, isUserCommand, isAttachmentMsg
    , isTextMsg :: VKStructs.VKMessage -> Maybe PureStructs.MessageType 

isCallBackMsg vkMsg = case VKStructs.cbPayload vkMsg of 
    Nothing -> Nothing 
    Just callback -> 
        let result = mbRep1 callback
                <|> mbRep2 callback 
                <|> mbRep3 callback
                <|> mbRep4 callback 
                <|> mbRep5 callback
        in case result of 
                Nothing -> Nothing 
                Just val -> Just $ PureStructs.MTCallbackQuery val 

isUserCommand vkMsg = case VKStructs.msgText vkMsg of  
    Nothing -> Nothing 
    Just txt -> case txt of 
        "/help" -> pure $ PureStructs.MTUserCommand PureStructs.Help
        "/repeat"  -> pure $ PureStructs.MTUserCommand PureStructs.Repeat
        _ -> Nothing 

isAttachmentMsg vkMsg = Nothing 

isTextMsg vkMsg = case VKStructs.msgText vkMsg of 
    Nothing -> Nothing 
    _ -> pure $ PureStructs.MTCommon "Message"

mbRep1 :: T.Text -> Maybe T.Text 
mbRep1 txt = if T.isInfixOf PureStructs.rep1 txt 
    then pure PureStructs.rep1 
    else Nothing 

mbRep2 :: T.Text -> Maybe T.Text 
mbRep2 txt = if T.isInfixOf PureStructs.rep2 txt 
    then pure PureStructs.rep2 
    else Nothing 

mbRep3 :: T.Text -> Maybe T.Text 
mbRep3 txt = if T.isInfixOf PureStructs.rep3 txt 
    then pure PureStructs.rep3 
    else Nothing 

mbRep4 :: T.Text -> Maybe T.Text 
mbRep4 txt = if T.isInfixOf PureStructs.rep4 txt 
    then pure PureStructs.rep4 
    else Nothing 

mbRep5 :: T.Text -> Maybe T.Text 
mbRep5 txt = if T.isInfixOf PureStructs.rep5 txt 
    then pure PureStructs.rep5 
    else Nothing 

baseParams :: VKStructs.VKMessage -> [PureStructs.Params] 
baseParams vkMsg = [PureStructs.ParamsNum "user_id" (VKStructs.from_id vkMsg)]

makeParams :: Config.Config 
    -> PureStructs.MessageType    
    -> VKStructs.VKMessage 
    -> Maybe [PureStructs.Params]
makeParams config (PureStructs.MTUserCommand PureStructs.Help) vkMsg = pure $ 
            (PureStructs.ParamsText "message" (Config.helpMessage config)) : baseParams vkMsg 
makeParams _ (PureStructs.MTUserCommand PureStructs.Repeat) vkMsg = pure $ baseParams vkMsg 
    <> [PureStructs.ParamsText "message" PureStructs.repeatText
        , PureStructs.ParamsJSON "keyboard" keyboard]     
makeParams _ (PureStructs.MTCommon "Message") vkMsg = do 
            txt <- VKStructs.msgText vkMsg       
            pure $ (PureStructs.ParamsText "message" txt) : baseParams vkMsg  
makeParams _ _ _ = Nothing 

decodeKeyboard  :: T.Text
decodeKeyboard = case (decode . encode) keyboard of 
    Nothing -> ""
    Just val -> val 

keyboard :: Value
keyboard = object ["inline" .= True 
    , "buttons" .= (((fmap . fmap) pureBtnToVKBtnAct PureStructs.buttons')) ]

pureBtnToVKBtnAct :: PureStructs.PureButtons -> VKStructs.BtnAction 
pureBtnToVKBtnAct btn@(PureStructs.PureButtons btnLabel _) = 
    VKStructs.BtnAction $ VKStructs.VKButtons "callback" ((T.pack . C8.unpack) $ mkLBS btn) btnLabel

mkLBS :: PureStructs.PureButtons -> BSL.ByteString
mkLBS (PureStructs.PureButtons btnLabel btnData) = encode $ object [btnLabel .= btnData]

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
    