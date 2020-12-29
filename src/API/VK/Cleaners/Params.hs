module API.VK.Cleaners.Params where

import qualified Data.Text as T 
import qualified API.VK.Structs as VKStructs 
import qualified Logic.PureStructs as PureStructs
import qualified Config.Config as Config 
import API.VK.Cleaners.IsObjFunctions
    ( isLink, isSticker, isMedia ) 
import API.VK.Cleaners.Keyboard ( keyboard ) 

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
    pure $ setMessageParam (VKStructs.msgText vkMsg) 
        <> setMaybeDoubleParam "lat" ((fmap VKStructs.latitude) . (fmap VKStructs.gCoordinates) . VKStructs.geo) vkMsg 
        <> setMaybeDoubleParam "long" ((fmap VKStructs.longitude) . (fmap VKStructs.gCoordinates) . VKStructs.geo) vkMsg 
        <> baseParams vkMsg
makeParams _ (PureStructs.MTCommon "Fwd") vkMsg = do 
    let msgIds = getFwdMsgIds (VKStructs.fwdMessages vkMsg)
    pure $ setMessageParam (VKStructs.msgText vkMsg)  
        <> [PureStructs.ParamsText "forward_messages" msgIds]
        <> baseParams vkMsg
makeParams _ (PureStructs.MTCommon "Attachment") vkMsg = do 
    let attachParams = attachmentListParams vkMsg (VKStructs.attachments vkMsg)
    makeAttachParams vkMsg attachParams 
makeParams _ _ _ = Nothing 

makeAttachParams :: VKStructs.VKMessage -> [PureStructs.Params]
    -> Maybe [PureStructs.Params]
makeAttachParams vkMsg [] = pure $ setMsgParamNotEmpty vkMsg <> baseParams vkMsg 
makeAttachParams vkMsg _ =pure $ setMessageParam (VKStructs.msgText vkMsg)  
    <> baseParams vkMsg  
    <> attachmentListParams vkMsg (VKStructs.attachments vkMsg)

setMsgParamNotEmpty :: VKStructs.VKMessage -> [PureStructs.Params]
setMsgParamNotEmpty vkMsg = case setMessageParam (VKStructs.msgText vkMsg) of 
    [] -> [PureStructs.ParamsText "message" "The attachment you sent is not supperted by this bot"]
    params -> params 

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
makeStickerParams (VKStructs.VKSticker sticker : _) = [PureStructs.ParamsText "sticker_id" $ (T.pack . show) sticker]
makeStickerParams _ = [] 

makeMediaParams :: [VKStructs.AObject] -> [PureStructs.Params]
makeMediaParams [] = [] 
makeMediaParams media = 
    [PureStructs.ParamsText "attachment" $ getMediaInfo media]

getMediaInfo :: [VKStructs.AObject] -> T.Text
getMediaInfo media = T.intercalate "," $ makeMediaInfo <$> media 

makeMediaInfo :: VKStructs.AObject -> T.Text 
makeMediaInfo (VKStructs.VKAudio audioId ownerId) = 
    "audio" <> getOwnerIdItemId ownerId audioId 
makeMediaInfo (VKStructs.VKVideo videoId ownerId accessKey) = 
    "video" <> getOwnerIdItemIdAccessKey ownerId videoId accessKey 
makeMediaInfo (VKStructs.VKWall wallId ownerId) = 
    "wall" <> getOwnerIdItemId ownerId wallId 
makeMediaInfo (VKStructs.VKMarket marketId ownerId) = 
    "market" <> getOwnerIdItemId ownerId marketId 
makeMediaInfo (VKStructs.VKPoll pollId ownerId) = 
    "poll" <> getOwnerIdItemId ownerId pollId 
makeMediaInfo _ = ""

getOwnerIdItemId :: VKStructs.OwnerID -> VKStructs.ItemID -> T.Text
getOwnerIdItemId ownerId itemId = (T.pack . show) ownerId 
    <> "_" 
    <> (T.pack . show) itemId 

getOwnerIdItemIdAccessKey :: VKStructs.OwnerID -> VKStructs.ItemID 
    -> VKStructs.AccessKey -> T.Text
getOwnerIdItemIdAccessKey ownerId itemId accessKey = 
    (T.pack . show) ownerId <> "_" 
        <> (T.pack . show) itemId <> "_"
        <> accessKey

getFwdMsgIds :: Maybe [VKStructs.VKMessage] -> T.Text
getFwdMsgIds Nothing = ""
getFwdMsgIds (Just []) = ""
getFwdMsgIds (Just [x]) = T.pack . show $ VKStructs.id x
getFwdMsgIds (Just (x:xs)) = (T.pack . show $ VKStructs.id x) <> ","
    <> (getFwdMsgIds (Just xs))

setMessageParam :: Maybe T.Text -> [PureStructs.Params]
setMessageParam Nothing = []
setMessageParam (Just "") = []
setMessageParam (Just msg) = [PureStructs.ParamsText "message" msg]

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
