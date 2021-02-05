module API.VK.Cleaners.Params.AttachParams where

import API.VK.Cleaners.Params.BaseParams (baseParams)
import API.VK.Cleaners.Params.IsObjFunctions
  ( isLink,
    isMedia,
    isSticker,
  )
import API.VK.Cleaners.Params.SharedFunctions (setMessageParam)
import qualified API.VK.Structs.Exports as VKStructs
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

makeAttachParams ::
  VKStructs.VKMessage ->
  [PureStructs.Params] ->
  Maybe [PureStructs.Params]
makeAttachParams vkMsg [] = pure $ setMsgParamNotEmpty vkMsg <> baseParams vkMsg
makeAttachParams vkMsg _ =
  pure $
    setMessageParam (VKStructs.msg_text vkMsg)
      <> baseParams vkMsg
      <> attachmentListParams vkMsg (VKStructs.attachments vkMsg)

setMsgParamNotEmpty :: VKStructs.VKMessage -> [PureStructs.Params]
setMsgParamNotEmpty vkMsg = case setMessageParam (VKStructs.msg_text vkMsg) of
  [] -> [PureStructs.ParamsText "message" "The attachment you sent is not supperted by this bot"]
  params -> params

attachmentListParams :: VKStructs.VKMessage -> Maybe [VKStructs.Attachment] -> [PureStructs.Params]
attachmentListParams _ Nothing = []
attachmentListParams _ (Just []) = []
attachmentListParams vkMsg (Just attachments) =
  let links = filter isLink (VKStructs.attachment_object <$> attachments)
      stickers = filter isSticker (VKStructs.attachment_object <$> attachments)
      media = filter isMedia (VKStructs.attachment_object <$> attachments)
   in makeLinkParams vkMsg links
        <> makeStickerParams stickers
        <> makeMediaParams media

makeLinkParams :: VKStructs.VKMessage -> [VKStructs.AObject] -> [PureStructs.Params]
makeLinkParams _ [] = []
makeLinkParams vkMsg links = case VKStructs.msg_text vkMsg of
  Nothing -> [PureStructs.ParamsText "message" $ getLinks links]
  Just "" -> [PureStructs.ParamsText "message" $ getLinks links]
  _ -> []

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
getOwnerIdItemId ownerId itemId =
  (T.pack . show) ownerId
    <> "_"
    <> (T.pack . show) itemId

getOwnerIdItemIdAccessKey ::
  VKStructs.OwnerID ->
  VKStructs.ItemID ->
  VKStructs.AccessKey ->
  T.Text
getOwnerIdItemIdAccessKey ownerId itemId accessKey =
  (T.pack . show) ownerId <> "_"
    <> (T.pack . show) itemId
    <> "_"
    <> accessKey
