module API.VK.Functions.Params.Message where

import qualified API.PureStructs.Exports as PureStructs
import qualified API.VK.Structs.Exports as VK
import qualified Data.Text as T

basicParams :: VK.Message -> [PureStructs.Params]
basicParams msg = [PureStructs.ParamsNum "user_id" (VK.from_id msg)]

type Key = T.Text

buildMessageParam :: Maybe T.Text -> [PureStructs.Params]
buildMessageParam Nothing = []
buildMessageParam (Just "") = []
buildMessageParam (Just msg) = [PureStructs.ParamsText "message" msg]

buildMaybeTextParam ::
  Key ->
  (VK.Message -> Maybe T.Text) ->
  VK.Message ->
  [PureStructs.Params]
buildMaybeTextParam key field msg = case field msg of
  Nothing -> []
  Just txt -> [PureStructs.ParamsText key txt]

buildMaybeDoubleParam ::
  Key ->
  (VK.Message -> Maybe Double) ->
  VK.Message ->
  [PureStructs.Params]
buildMaybeDoubleParam key field msg = case field msg of
  Nothing -> []
  Just val -> [PureStructs.ParamsDouble key val]
