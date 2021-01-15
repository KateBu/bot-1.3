module API.VK.Cleaners.MsgTypes.Callback where

import qualified API.VK.Structs as VKStructs
import Control.Applicative (Alternative ((<|>)))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

mbCallBackMsg :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
mbCallBackMsg vkMsg =
  PureStructs.MTCallbackQuery <$> mbNewRep (fromMaybe "" $ VKStructs.cbPayload vkMsg)

mbNewRep :: T.Text -> Maybe T.Text
mbNewRep callback =
  mbRep1 callback
    <|> mbRep2 callback
    <|> mbRep3 callback
    <|> mbRep4 callback
    <|> mbRep5 callback

mbRep1 :: T.Text -> Maybe T.Text
mbRep1 txt =
  if T.isInfixOf PureStructs.rep1 txt
    then pure PureStructs.rep1
    else Nothing

mbRep2 :: T.Text -> Maybe T.Text
mbRep2 txt =
  if T.isInfixOf PureStructs.rep2 txt
    then pure PureStructs.rep2
    else Nothing

mbRep3 :: T.Text -> Maybe T.Text
mbRep3 txt =
  if T.isInfixOf PureStructs.rep3 txt
    then pure PureStructs.rep3
    else Nothing

mbRep4 :: T.Text -> Maybe T.Text
mbRep4 txt =
  if T.isInfixOf PureStructs.rep4 txt
    then pure PureStructs.rep4
    else Nothing

mbRep5 :: T.Text -> Maybe T.Text
mbRep5 txt =
  if T.isInfixOf PureStructs.rep5 txt
    then pure PureStructs.rep5
    else Nothing
