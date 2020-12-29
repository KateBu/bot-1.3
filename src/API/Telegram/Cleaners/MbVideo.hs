module API.Telegram.Cleaners.MbVideo where

import qualified API.Telegram.Structs as TStructs 
import qualified Logic.PureStructs as PureStructs 
import API.Telegram.Cleaners.GetParams ( basicParams )

mbVideo :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo 
    -> Maybe PureStructs.PureMessage     
mbVideo uid chid mInfo = 
    let mbVid = TStructs.video mInfo
    in maybe Nothing (mbVideo' uid chid mInfo) mbVid 

mbVideo' :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo -> TStructs.TelVideo
    -> Maybe PureStructs.PureMessage    
mbVideo' uid chid mInfo video = pure $ PureStructs.PureMessage 
    (PureStructs.MTCommon "Video")
    uid
    (Just chid)
    (Just $ basicParams chid mInfo 
        <> [PureStructs.ParamsText "video" (TStructs.video_file_id video)])