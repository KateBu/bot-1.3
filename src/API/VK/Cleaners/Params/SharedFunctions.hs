module API.VK.Cleaners.Params.SharedFunctions where

import qualified API.VK.Structs.Internals as VKStructs
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

setMessageParam :: Maybe T.Text -> [PureStructs.Params]
setMessageParam Nothing = []
setMessageParam (Just "") = []
setMessageParam (Just msg) = [PureStructs.ParamsText "message" msg]

setMaybeTextParam ::
  T.Text ->
  (VKStructs.VKMessage -> Maybe T.Text) ->
  VKStructs.VKMessage ->
  [PureStructs.Params]
setMaybeTextParam key field vkMsg = case field vkMsg of
  Nothing -> []
  Just txt -> [PureStructs.ParamsText key txt]

setMaybeDoubleParam ::
  T.Text ->
  (VKStructs.VKMessage -> Maybe Double) ->
  VKStructs.VKMessage ->
  [PureStructs.Params]
setMaybeDoubleParam key field vkMsg = case field vkMsg of
  Nothing -> []
  Just val -> [PureStructs.ParamsDouble key val]
