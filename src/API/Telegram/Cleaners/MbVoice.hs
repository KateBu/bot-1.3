module API.Telegram.Cleaners.MbVoice where

import qualified API.Telegram.TStructs.MessageInfo as TStructs 
import qualified Logic.PureStructs as PureStructs 
import API.Telegram.Cleaners.GetParams ( basicParams )

mbVoice :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo 
    -> Maybe PureStructs.PureMessage  
mbVoice uid chid mInfo = 
    let mbV = TStructs.voice mInfo 
    in maybe Nothing (mbVoice' uid chid mInfo) mbV 

mbVoice' :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo -> TStructs.TelVoice
    -> Maybe PureStructs.PureMessage  
mbVoice' uid chid mInfo voice = pure $ PureStructs.PureMessage 
    (PureStructs.MTCommon "Voice")
    uid 
    (Just chid)
    (Just $ basicParams chid mInfo 
        <> [PureStructs.ParamsText "voice" (TStructs.voice_file_id voice)])