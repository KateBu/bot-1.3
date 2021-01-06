module API.Telegram.Cleaners.MbVenue where

import qualified Data.Text as T 
import qualified API.Telegram.TStructs.MessageInfo as TStructs 
import qualified Logic.PureStructs as PureStructs 
import API.Telegram.Cleaners.GetParams ( basicParams )

mbVenue :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo 
    -> Maybe PureStructs.PureMessage  
mbVenue uid chid mInfo = 
    let mbVen = TStructs.venue mInfo
    in maybe Nothing (mbVenue' uid chid mInfo) mbVen 

mbVenue' :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo -> TStructs.TelVenue
    -> Maybe PureStructs.PureMessage 
mbVenue' uid chid mInfo venue = pure $ PureStructs.PureMessage 
    (PureStructs.MTCommon "Venue")
    uid 
    (Just chid)
    (Just $ basicParams chid mInfo 
    <> [PureStructs.ParamsText "latitude" ((T.pack . show) $ TStructs.v_latitude venue)
        , PureStructs.ParamsText "longitude" ((T.pack . show) $ TStructs.v_longitude venue)
        , PureStructs.ParamsText "title" (TStructs.v_title venue)      
        , PureStructs.ParamsText "address" (TStructs.v_address venue)      
        ] )