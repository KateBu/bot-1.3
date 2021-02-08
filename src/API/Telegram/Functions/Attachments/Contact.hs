module API.Telegram.Functions.Attachments.Contact where

import API.Telegram.Functions.Params (basicParams, buildTextParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.Structs as PureStructs

buildContactMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
buildContactMessage updatId chatId msgInfo = do
  contactInfo <- TStructs.contact msgInfo
  buildContactMessage' updatId chatId msgInfo contactInfo

buildContactMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelContact ->
  Maybe PureStructs.PureMessage
buildContactMessage' updatId chatId msgInfo contactInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Contact")
      updatId
      (Just chatId)
      (buildContactParams chatId msgInfo contactInfo)

buildContactParams :: PureStructs.ChatID -> TStructs.MessageInfo -> TStructs.TelContact -> Maybe [PureStructs.Params]
buildContactParams chatiId msgInfo contactInfo =
  Just $
    basicParams chatiId msgInfo
      <> [ PureStructs.ParamsText "phone_number" (TStructs.phone_number contactInfo),
           PureStructs.ParamsText "first_name" (TStructs.first_name contactInfo)
         ]
      <> buildTextParams "last_name" (TStructs.last_name contactInfo)
      <> buildTextParams "vcard" (TStructs.vcard contactInfo)
