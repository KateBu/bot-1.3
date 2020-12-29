module API.Telegram.Cleaners.MbLocation where

import qualified Data.Text as T 
import qualified API.Telegram.Structs as TStructs 
import qualified Logic.PureStructs as PureStructs 
import API.Telegram.Cleaners.GetParams ( basicParams )

mbLocation :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo 
    -> Maybe PureStructs.PureMessage    
mbLocation uid chid mInfo = 
    let mbLoc = TStructs.location mInfo 
    in maybe Nothing (mbLocation' uid chid mInfo) mbLoc 

mbLocation' :: PureStructs.UpdateID -> PureStructs.ChatID 
    -> TStructs.MessageInfo -> TStructs.TelLocation
    -> Maybe PureStructs.PureMessage  
mbLocation' uid chid mInfo location = pure $ PureStructs.PureMessage 
    (PureStructs.MTCommon "Location")
    uid 
    (Just chid)
    (Just $ basicParams chid mInfo 
    <> [PureStructs.ParamsText "latitude" ((T.pack . show) $ TStructs.latitude location)
        , PureStructs.ParamsText "longitude" ((T.pack . show) $ TStructs.longitude location) 
    ])