module API.Telegram.Cleaners.Attachments.MbVenue where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

mbVenue ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbVenue uid chid mInfo = do 
  venueInfo <- TStructs.venue mInfo 
  mbVenue' uid chid mInfo venueInfo 

mbVenue' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelVenue ->
  Maybe PureStructs.PureMessage
mbVenue' uid chid mInfo venue =
  pure $
    PureStructs.PureMessage
      (PureStructs.MTCommon "Venue")
      uid
      (Just chid)
      (venueParams chid mInfo venue)

venueParams :: PureStructs.ChatID -> TStructs.MessageInfo -> TStructs.TelVenue -> Maybe [PureStructs.Params]
venueParams chid mInfo venue =
  Just $
    basicParams chid mInfo
      <> [ PureStructs.ParamsText "latitude" ((T.pack . show) $ TStructs.v_latitude venue),
           PureStructs.ParamsText "longitude" ((T.pack . show) $ TStructs.v_longitude venue),
           PureStructs.ParamsText "title" (TStructs.v_title venue),
           PureStructs.ParamsText "address" (TStructs.v_address venue)
         ]
