module API.VK.Functions.Params.AttachmentParams
  ( buildAttachmentListParams,
    buildAttachmentParams,
  )
where

import API.VK.Functions.Params.MessageParams (basicParams, buildMessageParam)
import qualified API.VK.Structs.Exports as VKStructs
import qualified Data.Text as T
import qualified Logic.Structs as PureStructs

buildAttachmentParams ::
  VKStructs.VKMessage ->
  [PureStructs.Params] ->
  Maybe [PureStructs.Params]
buildAttachmentParams vkMsg [] = pure $ buildUnknowmAttachmentParams vkMsg <> basicParams vkMsg
buildAttachmentParams vkMsg _ =
  pure $
    buildMessageParam (VKStructs.msg_text vkMsg)
      <> basicParams vkMsg
      <> buildAttachmentListParams vkMsg (VKStructs.attachments vkMsg)

buildUnknowmAttachmentParams :: VKStructs.VKMessage -> [PureStructs.Params]
buildUnknowmAttachmentParams vkMsg = case buildMessageParam (VKStructs.msg_text vkMsg) of
  [] -> [PureStructs.ParamsText "message" "The attachment you sent is not supperted by this bot"]
  params -> params

buildAttachmentListParams :: VKStructs.VKMessage -> Maybe [VKStructs.Attachment] -> [PureStructs.Params]
buildAttachmentListParams _ Nothing = []
buildAttachmentListParams _ (Just []) = []
buildAttachmentListParams vkMsg (Just attachments) =
  let links = filter isLink (VKStructs.attachment_object <$> attachments)
      stickers = filter isSticker (VKStructs.attachment_object <$> attachments)
      media = filter isMedia (VKStructs.attachment_object <$> attachments)
   in buildLinkParams vkMsg links
        <> buildStickerParams stickers
        <> buildMediaParams media

buildLinkParams :: VKStructs.VKMessage -> [VKStructs.AObject] -> [PureStructs.Params]
buildLinkParams _ [] = []
buildLinkParams vkMsg links = case VKStructs.msg_text vkMsg of
  Nothing -> [PureStructs.ParamsText "message" $ getLinks links]
  Just "" -> [PureStructs.ParamsText "message" $ getLinks links]
  _ -> []

getLinks :: [VKStructs.AObject] -> T.Text
getLinks [] = ""
getLinks (VKStructs.VKLink url : links) =
  url <> " " <> getLinks links
getLinks _ = ""

buildStickerParams :: [VKStructs.AObject] -> [PureStructs.Params]
buildStickerParams (VKStructs.VKSticker sticker : _) = [PureStructs.ParamsText "sticker_id" $ (T.pack . show) sticker]
buildStickerParams _ = []

buildMediaParams :: [VKStructs.AObject] -> [PureStructs.Params]
buildMediaParams [] = []
buildMediaParams media =
  [PureStructs.ParamsText "attachment" $ buildMediaInfo media]

buildMediaInfo :: [VKStructs.AObject] -> T.Text
buildMediaInfo media = T.intercalate "," $ buildMediaInfo' <$> media

buildMediaInfo' :: VKStructs.AObject -> T.Text
buildMediaInfo' (VKStructs.VKAudio audioId ownerId) =
  "audio" <> buildOwnerIdItemId ownerId audioId
buildMediaInfo' (VKStructs.VKVideo videoId ownerId accessKey) =
  "video" <> buildOwnerIdItemIdAccessKey ownerId videoId accessKey
buildMediaInfo' (VKStructs.VKWall wallId ownerId) =
  "wall" <> buildOwnerIdItemId ownerId wallId
buildMediaInfo' (VKStructs.VKMarket marketId ownerId) =
  "market" <> buildOwnerIdItemId ownerId marketId
buildMediaInfo' (VKStructs.VKPoll pollId ownerId) =
  "poll" <> buildOwnerIdItemId ownerId pollId
buildMediaInfo' _ = ""

buildOwnerIdItemId :: VKStructs.OwnerID -> VKStructs.ItemID -> T.Text
buildOwnerIdItemId ownerId itemId =
  (T.pack . show) ownerId
    <> "_"
    <> (T.pack . show) itemId

buildOwnerIdItemIdAccessKey ::
  VKStructs.OwnerID ->
  VKStructs.ItemID ->
  VKStructs.AccessKey ->
  T.Text
buildOwnerIdItemIdAccessKey ownerId itemId accessKey =
  (T.pack . show) ownerId <> "_"
    <> (T.pack . show) itemId
    <> "_"
    <> accessKey

isLink, isSticker, isMedia :: VKStructs.AObject -> Bool
isLink (VKStructs.VKLink _) = True
isLink _ = False
isSticker (VKStructs.VKSticker _) = True
isSticker _ = False
isMedia (VKStructs.VKAudio _ _) = True
isMedia VKStructs.VKVideo {} = True
isMedia (VKStructs.VKWall _ _) = True
isMedia (VKStructs.VKMarket _ _) = True
isMedia (VKStructs.VKPoll _ _) = True
isMedia _ = False
