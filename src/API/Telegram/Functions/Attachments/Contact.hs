module API.Telegram.Functions.Attachments.Contact (buildContactMessage) where

import qualified API.PureStructs.Exports as PureStructs
import API.Telegram.Functions.Params (buildBasicParams, buildTextParams)
import qualified API.Telegram.Structs.MessageInfo as Telegram

buildContactMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Maybe PureStructs.PureMessage
buildContactMessage updatId chatId msgInfo = do
  contactInfo <- Telegram.contact msgInfo
  buildContactMessage' updatId chatId msgInfo contactInfo

buildContactMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Telegram.Contact ->
  Maybe PureStructs.PureMessage
buildContactMessage' updatId chatId msgInfo contactInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Contact")
      updatId
      (Just chatId)
      (buildContactParams chatId msgInfo contactInfo)

buildContactParams :: PureStructs.ChatID -> Telegram.MessageInfo -> Telegram.Contact -> Maybe [PureStructs.Params]
buildContactParams chatiId msgInfo contactInfo =
  Just $
    buildBasicParams chatiId msgInfo
      <> [ PureStructs.ParamsText "phone_number" (Telegram.phone_number contactInfo),
           PureStructs.ParamsText "first_name" (Telegram.first_name contactInfo)
         ]
      <> buildTextParams "last_name" (Telegram.last_name contactInfo)
      <> buildTextParams "vcard" (Telegram.vcard contactInfo)
