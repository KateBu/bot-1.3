module API.Telegram.Cleaners.MbPhoto where

import qualified API.Telegram.Structs as TStructs 
import qualified Logic.PureStructs as PureStructs 
import API.Telegram.Cleaners.GetParams ( basicParams )

mbPhoto :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo 
    -> Maybe PureStructs.PureMessage 
mbPhoto uid chid mInfo = 
    let mbPh = TStructs.photo mInfo
    in maybe Nothing (mbPhoto' uid chid mInfo) mbPh 

mbPhoto' :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo -> [TStructs.TelPhoto] 
    -> Maybe PureStructs.PureMessage 
mbPhoto' uid chid mInfo photo = pure $ PureStructs.PureMessage 
    (PureStructs.MTCommon "Photo")
    uid 
    (Just chid)
    (Just $ basicParams chid mInfo 
    <> getPhotoParams photo)    

getPhotoParams :: [TStructs.TelPhoto] -> [PureStructs.Params]
getPhotoParams [] = []
getPhotoParams (x:_) = [PureStructs.ParamsText "photo" (TStructs.photo_file_id x)]