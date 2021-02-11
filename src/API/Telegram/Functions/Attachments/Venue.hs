module API.Telegram.Functions.Attachments.Venue (buildVenueMessage) where

import qualified API.PureStructs.Exports as PureStructs
import API.Telegram.Functions.Params (buildBasicParams)
import qualified API.Telegram.Structs.MessageInfo as Telegram
import qualified Data.Text as T

buildVenueMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Maybe PureStructs.PureMessage
buildVenueMessage updateId chatId msgInfo = do
  venueInfo <- Telegram.venue msgInfo
  buildVenueMessage' updateId chatId msgInfo venueInfo

buildVenueMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Telegram.Venue ->
  Maybe PureStructs.PureMessage
buildVenueMessage' updateId chatId msgInfo venueInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Venue")
      updateId
      (Just chatId)
      (buildVenueParams chatId msgInfo venueInfo)

buildVenueParams :: PureStructs.ChatID -> Telegram.MessageInfo -> Telegram.Venue -> Maybe [PureStructs.Params]
buildVenueParams chatId msgInfo venueInfo =
  Just $
    buildBasicParams chatId msgInfo
      <> [ PureStructs.ParamsText "latitude" ((T.pack . show) $ Telegram.venue_latitude venueInfo),
           PureStructs.ParamsText "longitude" ((T.pack . show) $ Telegram.venue_longitude venueInfo),
           PureStructs.ParamsText "title" (Telegram.venue_title venueInfo),
           PureStructs.ParamsText "address" (Telegram.venue_address venueInfo)
         ]
