module API.Telegram.Cleaners.MbContact where

import qualified API.Telegram.TStructs.MessageInfo as TStructs 
import qualified Logic.PureStructs as PureStructs 
import API.Telegram.Cleaners.GetParams ( basicParams )
import API.Telegram.Cleaners.MakeMbParams ( makeMaybeTextParams )

mbContact :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo 
    -> Maybe PureStructs.PureMessage     
mbContact uid chid mInfo = 
    let mbCon = TStructs.contact mInfo 
    in maybe Nothing (mbContact' uid chid mInfo) mbCon 

mbContact' :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo -> TStructs.TelContact
    -> Maybe PureStructs.PureMessage     
mbContact' uid chid mInfo contact = pure $ PureStructs.PureMessage 
    (PureStructs.MTCommon "Contact")
    uid 
    (Just chid)
    (Just $ basicParams chid mInfo 
    <> [PureStructs.ParamsText "phone_number" (TStructs.phone_number contact)
        , PureStructs.ParamsText "first_name" (TStructs.first_name contact)            
        ] 
        <> makeMaybeTextParams "last_name" (TStructs.last_name contact)
    <> makeMaybeTextParams "vcard" (TStructs.vcard contact)
    )
