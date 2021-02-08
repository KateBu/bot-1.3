module API.VK.Functions.Params.MessageParams where

import qualified API.VK.Structs.Exports as VKStructs
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

basicParams :: VKStructs.VKMessage -> [PureStructs.Params]
basicParams vkMsg = [PureStructs.ParamsNum "user_id" (VKStructs.from_id vkMsg)]

type Key = T.Text

buildMessageParam :: Maybe T.Text -> [PureStructs.Params]
buildMessageParam Nothing = []
buildMessageParam (Just "") = []
buildMessageParam (Just msg) = [PureStructs.ParamsText "message" msg]

buildMaybeTextParam ::
  Key ->
  (VKStructs.VKMessage -> Maybe T.Text) ->
  VKStructs.VKMessage ->
  [PureStructs.Params]
buildMaybeTextParam key field vkMsg = case field vkMsg of
  Nothing -> []
  Just txt -> [PureStructs.ParamsText key txt]

buildMaybeDoubleParam ::
  Key ->
  (VKStructs.VKMessage -> Maybe Double) ->
  VKStructs.VKMessage ->
  [PureStructs.Params]
buildMaybeDoubleParam key field vkMsg = case field vkMsg of
  Nothing -> []
  Just val -> [PureStructs.ParamsDouble key val]
