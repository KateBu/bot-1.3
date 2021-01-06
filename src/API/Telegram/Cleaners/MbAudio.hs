module API.Telegram.Cleaners.MbAudio where

import qualified API.Telegram.TStructs.MessageInfo as TStructs 
import qualified Logic.PureStructs as PureStructs 
import API.Telegram.Cleaners.GetParams ( basicParams )

mbAudio :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo -> Maybe PureStructs.PureMessage    
mbAudio uid chid mInfo = 
    let mbAud = TStructs.audio mInfo 
    in maybe Nothing (mbAudio' uid chid mInfo) mbAud 

mbAudio' :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo -> TStructs.TelAudio
    -> Maybe PureStructs.PureMessage      
mbAudio' uid chid mInfo aud = pure $ PureStructs.PureMessage
        (PureStructs.MTCommon "Audio")
        uid 
        (Just chid)
        (Just $ basicParams chid mInfo 
            <> [PureStructs.ParamsText "audio" (TStructs.audio_id aud)])   
   
