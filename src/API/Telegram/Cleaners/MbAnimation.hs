module API.Telegram.Cleaners.MbAnimation where

import qualified API.Telegram.TStructs.MessageInfo as TStructs 
import qualified Logic.PureStructs as PureStructs 
import API.Telegram.Cleaners.GetParams ( basicParams )

mbAnimation :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo -> Maybe PureStructs.PureMessage      
mbAnimation uid chid mInfo = 
    let mbAnim = TStructs.animation mInfo
    in maybe Nothing (mbAnimation' uid chid mInfo) mbAnim 

mbAnimation' :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo -> TStructs.TelAmination 
    -> Maybe PureStructs.PureMessage  
mbAnimation' uid chid mInfo anim = pure $ PureStructs.PureMessage 
    (PureStructs.MTCommon "Animation")
    uid
    (Just chid)
    (Just $ basicParams chid mInfo 
        <> [PureStructs.ParamsText "animation" (TStructs.animation_file_id anim)]) 