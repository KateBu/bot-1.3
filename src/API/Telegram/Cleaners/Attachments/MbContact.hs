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
mbContact uid chid mInfo =
  TStructs.contact mInfo >>= mbContact' uid chid mInfo

mbContact' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelContact ->
  Maybe PureStructs.PureMessage
mbContact' uid chid mInfo contact =
  pure $
    PureStructs.PureMessage
      (PureStructs.MTCommon "Contact")
      uid
      (Just chid)
      contactParams
  where
    contactParams =
      Just $
        basicParams chid mInfo
          <> [ PureStructs.ParamsText "phone_number" (TStructs.phone_number contact),
               PureStructs.ParamsText "first_name" (TStructs.first_name contact)
             ]
          <> makeMaybeTextParams "last_name" (TStructs.last_name contact)
          <> makeMaybeTextParams "vcard" (TStructs.vcard contact)
