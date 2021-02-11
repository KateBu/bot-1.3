module API.VK.Functions.Params.Attachment
  ( buildAttachmentListParams,
    buildAttachmentParams,
  )
where

import API.VK.Functions.Params.Message (basicParams, buildMessageParam)
import qualified API.VK.Structs.Exports as VK
import qualified Data.Text as T
import qualified Logic.Structs as PureStructs

buildAttachmentParams ::
  VK.Message ->
  [PureStructs.Params] ->
  Maybe [PureStructs.Params]
buildAttachmentParams msg [] = pure $ buildUnknowmAttachmentParams msg <> basicParams msg
buildAttachmentParams msg _ =
  pure $
    buildMessageParam (VK.msg_text msg)
      <> basicParams msg
      <> buildAttachmentListParams msg (VK.attachments msg)

buildUnknowmAttachmentParams :: VK.Message -> [PureStructs.Params]
buildUnknowmAttachmentParams msg = case buildMessageParam (VK.msg_text msg) of
  [] -> [PureStructs.ParamsText "message" "The attachment you sent is not supperted by this bot"]
  params -> params

buildAttachmentListParams :: VK.Message -> Maybe [VK.Attachment] -> [PureStructs.Params]
buildAttachmentListParams _ Nothing = []
buildAttachmentListParams _ (Just []) = []
buildAttachmentListParams msg (Just attachments) =
  let links = filter isLink (VK.attachment_object <$> attachments)
      stickers = filter isSticker (VK.attachment_object <$> attachments)
      media = filter isMedia (VK.attachment_object <$> attachments)
   in buildLinkParams msg links
        <> buildStickerParams stickers
        <> buildMediaParams media

buildLinkParams :: VK.Message -> [VK.AObject] -> [PureStructs.Params]
buildLinkParams _ [] = []
buildLinkParams msg links = case VK.msg_text msg of
  Nothing -> [PureStructs.ParamsText "message" $ getLinks links]
  Just "" -> [PureStructs.ParamsText "message" $ getLinks links]
  _ -> []

getLinks :: [VK.AObject] -> T.Text
getLinks [] = ""
getLinks (VK.Link url : links) =
  url <> " " <> getLinks links
getLinks _ = ""

buildStickerParams :: [VK.AObject] -> [PureStructs.Params]
buildStickerParams (VK.Sticker sticker : _) = [PureStructs.ParamsText "sticker_id" $ (T.pack . show) sticker]
buildStickerParams _ = []

buildMediaParams :: [VK.AObject] -> [PureStructs.Params]
buildMediaParams [] = []
buildMediaParams media =
  [PureStructs.ParamsText "attachment" $ buildMediaInfo media]

buildMediaInfo :: [VK.AObject] -> T.Text
buildMediaInfo media = T.intercalate "," $ buildMediaInfo' <$> media

buildMediaInfo' :: VK.AObject -> T.Text
buildMediaInfo' (VK.Audio audioId ownerId) =
  "audio" <> buildOwnerIdItemId ownerId audioId
buildMediaInfo' (VK.Video videoId ownerId accessKey) =
  "video" <> buildOwnerIdItemIdAccessKey ownerId videoId accessKey
buildMediaInfo' (VK.Wall wallId ownerId) =
  "wall" <> buildOwnerIdItemId ownerId wallId
buildMediaInfo' (VK.Market marketId ownerId) =
  "market" <> buildOwnerIdItemId ownerId marketId
buildMediaInfo' (VK.Poll pollId ownerId) =
  "poll" <> buildOwnerIdItemId ownerId pollId
buildMediaInfo' _ = ""

buildOwnerIdItemId :: VK.OwnerID -> VK.ItemID -> T.Text
buildOwnerIdItemId ownerId itemId =
  (T.pack . show) ownerId
    <> "_"
    <> (T.pack . show) itemId

buildOwnerIdItemIdAccessKey ::
  VK.OwnerID ->
  VK.ItemID ->
  VK.AccessKey ->
  T.Text
buildOwnerIdItemIdAccessKey ownerId itemId accessKey =
  (T.pack . show) ownerId <> "_"
    <> (T.pack . show) itemId
    <> "_"
    <> accessKey

isLink, isSticker, isMedia :: VK.AObject -> Bool
isLink (VK.Link _) = True
isLink _ = False
isSticker (VK.Sticker _) = True
isSticker _ = False
isMedia (VK.Audio _ _) = True
isMedia VK.Video {} = True
isMedia (VK.Wall _ _) = True
isMedia (VK.Market _ _) = True
isMedia (VK.Poll _ _) = True
isMedia _ = False
