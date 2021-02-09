module API.VK.Functions.MsgTypes.Callback (buildCallbackMessage) where

import qualified API.VK.Structs.Exports as VKStructs
import Control.Applicative (Alternative ((<|>)))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Logic.Structs as PureStructs

buildCallbackMessage :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
buildCallbackMessage vkMsg =
  PureStructs.MsgTypeCallbackQuery <$> mbNewRepeat (fromMaybe "" $ VKStructs.callback_payload vkMsg)

mbNewRepeat :: T.Text -> Maybe PureStructs.CallbackText
mbNewRepeat callback =
  mbRepeatNumber1 callback
    <|> mbRepeatNumber2 callback
    <|> mbRepeatNumber3 callback
    <|> mbRepeatNumber4 callback
    <|> mbRepeatNumber5 callback

mbRepeatNumber1 :: T.Text -> Maybe PureStructs.CallbackText
mbRepeatNumber1 txt =
  if T.isInfixOf PureStructs.setRepeat1 txt
    then pure PureStructs.setRepeat1
    else Nothing

mbRepeatNumber2 :: T.Text -> Maybe PureStructs.CallbackText
mbRepeatNumber2 txt =
  if T.isInfixOf PureStructs.setRepeat2 txt
    then pure PureStructs.setRepeat2
    else Nothing

mbRepeatNumber3 :: T.Text -> Maybe PureStructs.CallbackText
mbRepeatNumber3 txt =
  if T.isInfixOf PureStructs.setRepeat3 txt
    then pure PureStructs.setRepeat3
    else Nothing

mbRepeatNumber4 :: T.Text -> Maybe PureStructs.CallbackText
mbRepeatNumber4 txt =
  if T.isInfixOf PureStructs.setRepeat4 txt
    then pure PureStructs.setRepeat4
    else Nothing

mbRepeatNumber5 :: T.Text -> Maybe PureStructs.CallbackText
mbRepeatNumber5 txt =
  if T.isInfixOf PureStructs.setRepeat5 txt
    then pure PureStructs.setRepeat5
    else Nothing
