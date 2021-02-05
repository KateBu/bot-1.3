module API.Telegram.Cleaners.Attachments.MbContact where

import API.Telegram.Cleaners.GetParams (basicParams)
import API.Telegram.Cleaners.MakeMbParams (makeMaybeTextParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbContact ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbContact updatId chatId msgInfo = do
  contactInfo <- TStructs.contact msgInfo
  mbContact' updatId chatId msgInfo contactInfo

mbContact' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelContact ->
  Maybe PureStructs.PureMessage
mbContact' updatId chatId msgInfo contactInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Contact")
      updatId
      (Just chatId)
      (contactParams chatId msgInfo contactInfo)

contactParams :: PureStructs.ChatID -> TStructs.MessageInfo -> TStructs.TelContact -> Maybe [PureStructs.Params]
contactParams chatiId msgInfo contactInfo =
  Just $
    basicParams chatiId msgInfo
      <> [ PureStructs.ParamsText "phone_number" (TStructs.phone_number contactInfo),
           PureStructs.ParamsText "first_name" (TStructs.first_name contactInfo)
         ]
      <> makeMaybeTextParams "last_name" (TStructs.last_name contactInfo)
      <> makeMaybeTextParams "vcard" (TStructs.vcard contactInfo)
