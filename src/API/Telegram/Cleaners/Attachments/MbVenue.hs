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
mbVenue updateId chatId msgInfo = do
  venueInfo <- TStructs.venue msgInfo
  mbVenue' updateId chatId msgInfo venueInfo

mbVenue' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelVenue ->
  Maybe PureStructs.PureMessage
mbVenue' updateId chatId msgInfo venueInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Venue")
      updateId
      (Just chatId)
      (venueParams chatId msgInfo venueInfo)

venueParams :: PureStructs.ChatID -> TStructs.MessageInfo -> TStructs.TelVenue -> Maybe [PureStructs.Params]
venueParams chatId msgInfo venueInfo =
  Just $
    basicParams chatId msgInfo
      <> [ PureStructs.ParamsText "latitude" ((T.pack . show) $ TStructs.venue_latitude venueInfo),
           PureStructs.ParamsText "longitude" ((T.pack . show) $ TStructs.venue_longitude venueInfo),
           PureStructs.ParamsText "title" (TStructs.venue_title venueInfo),
           PureStructs.ParamsText "address" (TStructs.venue_address venueInfo)
         ]
