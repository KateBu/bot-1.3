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
            <|> isGeo vkMsg
            <|> isFwd vkMsg 
            <|> isAttachmentMsg vkMsg
            <|> isTextMsg vkMsg              
    
isCallBackMsg, isUserCommand
    , isAttachmentMsg, isTextMsg
    , isGeo, isFwd :: VKStructs.VKMessage -> Maybe PureStructs.MessageType 

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
    Just "" -> Nothing 
    Just txt -> case txt of 
        "/help" -> pure $ PureStructs.MTUserCommand PureStructs.Help
        "/repeat"  -> pure $ PureStructs.MTUserCommand PureStructs.Repeat
        _ -> Nothing 

isAttachmentMsg vkMsg = case VKStructs.attachments vkMsg of 
    Nothing -> Nothing 
    Just [] -> Nothing 
    _ -> pure $ PureStructs.MTCommon "Attachment"

isTextMsg vkMsg = case VKStructs.msgText vkMsg of 
    Nothing -> Nothing 
    _ -> pure $ PureStructs.MTCommon "Message"

isGeo vkMsg = case VKStructs.geo vkMsg of 
    Nothing -> Nothing 
    _ -> pure $ PureStructs.MTCommon "Geo"

isFwd vkMsg = case VKStructs.fwdMessages vkMsg of 
    Nothing -> Nothing 
    _ -> pure $ PureStructs.MTCommon "Fwd"

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
makeParams _ (PureStructs.MTCommon "Geo") vkMsg = 
    pure $ setMessageParam vkMsg 
        <> setMaybeDoubleParam "lat" ((fmap VKStructs.latitude) . (fmap VKStructs.gCoordinates) . VKStructs.geo) vkMsg 
        <> setMaybeDoubleParam "long" ((fmap VKStructs.longitude) . (fmap VKStructs.gCoordinates) . VKStructs.geo) vkMsg 
        <> baseParams vkMsg
makeParams _ (PureStructs.MTCommon "Fwd") vkMsg = do 
    let msgIds = getFwdMsgIds (VKStructs.fwdMessages vkMsg)
    pure $ setMessageParam vkMsg  
        <> [PureStructs.ParamsText "forward_messages" msgIds]
        <> baseParams vkMsg
makeParams _ (PureStructs.MTCommon "Attachment") vkMsg = 
    pure $ setMessageParam vkMsg  
        <> baseParams vkMsg  
        <> attachmentListParams vkMsg (VKStructs.attachments vkMsg)
makeParams _ _ _ = Nothing 

attachmentListParams :: VKStructs.VKMessage -> Maybe [VKStructs.Attachment] -> [PureStructs.Params]
attachmentListParams _ Nothing = [] 
attachmentListParams _ (Just []) = [] 
attachmentListParams vkMsg (Just attachments) = 
    let 
        links = filter isLink (VKStructs.aObject <$> attachments) 
        stickers = filter isSticker (VKStructs.aObject <$> attachments)  
        media = filter isMedia (VKStructs.aObject <$> attachments)  
    in makeLinkParams vkMsg links 
        <> makeStickerParams stickers 
        <> makeMediaParams media 

makeLinkParams :: VKStructs.VKMessage -> [VKStructs.AObject] -> [PureStructs.Params]
makeLinkParams _ [] = []
makeLinkParams vkMsg links = case VKStructs.msgText vkMsg of 
    Nothing -> [PureStructs.ParamsText "message" $ getLinks links]
    Just "" -> [PureStructs.ParamsText "message" $ getLinks links]
    _ ->  []

getLinks :: [VKStructs.AObject] -> T.Text
getLinks [] = ""
getLinks (VKStructs.VKLink url : links) = 
    url <> " " <> getLinks links 
getLinks _ = ""

makeStickerParams :: [VKStructs.AObject] -> [PureStructs.Params]
makeStickerParams [] = [] 
makeStickerParams (VKStructs.VKSticker sticker : _) = [PureStructs.ParamsText "sticker_id" $ (T.pack . show) sticker]

makeMediaParams :: [VKStructs.AObject] -> [PureStructs.Params]
makeMediaParams [] = [] 
makeMediaParams media = 
    [PureStructs.ParamsText "attachment" $ getMediaInfo media]

getMediaInfo :: [VKStructs.AObject] -> T.Text
getMediaInfo media = T.intercalate "," $ makeMediaInfo <$> media 

makeMediaInfo :: VKStructs.AObject -> T.Text 
makeMediaInfo (VKStructs.VKAudio audioId ownerId) = 
    "audio" <> (T.pack . show) ownerId <> "_" <> (T.pack . show) audioId
makeMediaInfo (VKStructs.VKVideo videoId ownerId accessKey) = 
    "video" <> (T.pack . show) ownerId <> "_" 
        <> (T.pack . show) videoId <> "_"
        <> accessKey
makeMediaInfo (VKStructs.VKWall wallId ownerId) = 
    "wall" <> (T.pack . show) ownerId <> "_" 
        <> (T.pack . show) wallId
makeMediaInfo _ = ""

isLink, isSticker, isMedia :: VKStructs.AObject -> Bool 
isLink (VKStructs.VKLink _) = True 
isLink _ = False 

isSticker (VKStructs.VKSticker _) = True 
isSticker _ = False 

isMedia (VKStructs.VKAudio _ _) = True 
isMedia (VKStructs.VKVideo _ _ _) = True 
isMedia (VKStructs.VKWall _ _) = True 
isMedia _ = False 

getFwdMsgIds :: Maybe [VKStructs.VKMessage] -> T.Text
getFwdMsgIds Nothing = ""
getFwdMsgIds (Just []) = ""
getFwdMsgIds (Just [x]) = T.pack . show $ VKStructs.id x
getFwdMsgIds (Just (x:xs)) = (T.pack . show $ VKStructs.id x) <> ","
    <> (getFwdMsgIds (Just xs))

setMessageParam :: VKStructs.VKMessage -> [PureStructs.Params]
setMessageParam vkMsg = case VKStructs.msgText vkMsg of 
    Nothing -> []
    Just "" -> []
    Just msg -> [PureStructs.ParamsText "message" msg]

setMaybeTextParam :: T.Text
    -> (VKStructs.VKMessage -> Maybe T.Text) 
    -> VKStructs.VKMessage 
    -> [PureStructs.Params]
setMaybeTextParam key field vkMsg = case field vkMsg of 
    Nothing -> [] 
    Just txt -> [PureStructs.ParamsText key txt]

setMaybeDoubleParam :: T.Text
    -> (VKStructs.VKMessage -> Maybe Double) 
    -> VKStructs.VKMessage 
    -> [PureStructs.Params]
setMaybeDoubleParam key field vkMsg = case field vkMsg of 
    Nothing -> [] 
    Just val -> [PureStructs.ParamsDouble key val]

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
    