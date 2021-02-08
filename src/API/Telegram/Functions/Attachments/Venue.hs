module API.Telegram.Functions.Attachments.Venue where

import API.Telegram.Functions.BasicParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

buildVenueMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
buildVenueMessage updateId chatId msgInfo = do
  venueInfo <- TStructs.venue msgInfo
  buildVenueMessage' updateId chatId msgInfo venueInfo

buildVenueMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelVenue ->
  Maybe PureStructs.PureMessage
buildVenueMessage' updateId chatId msgInfo venueInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Venue")
      updateId
      (Just chatId)
      (buildVenueParams chatId msgInfo venueInfo)

buildVenueParams :: PureStructs.ChatID -> TStructs.MessageInfo -> TStructs.TelVenue -> Maybe [PureStructs.Params]
buildVenueParams chatId msgInfo venueInfo =
  Just $
    basicParams chatId msgInfo
      <> [ PureStructs.ParamsText "latitude" ((T.pack . show) $ TStructs.venue_latitude venueInfo),
           PureStructs.ParamsText "longitude" ((T.pack . show) $ TStructs.venue_longitude venueInfo),
           PureStructs.ParamsText "title" (TStructs.venue_title venueInfo),
           PureStructs.ParamsText "address" (TStructs.venue_address venueInfo)
         ]
