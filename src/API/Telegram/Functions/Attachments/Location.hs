module API.Telegram.Functions.Attachments.Location (buildLocationMessage) where

import qualified API.PureStructs.Exports as PureStructs
import API.Telegram.Functions.Params (buildBasicParams)
import qualified API.Telegram.Structs.MessageInfo as Telegram
import qualified Data.Text as T

buildLocationMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Maybe PureStructs.PureMessage
buildLocationMessage updateId chatId msgInfo = do
  locationInfo <- Telegram.location msgInfo
  buildLocationMessage' updateId chatId msgInfo locationInfo

buildLocationMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Telegram.Location ->
  Maybe PureStructs.PureMessage
buildLocationMessage' updateId chatId msgInfo locationInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Location")
      updateId
      (Just chatId)
      locationParams
  where
    locationParams =
      Just $
        buildBasicParams chatId msgInfo
          <> [ PureStructs.ParamsText "latitude" ((T.pack . show) $ Telegram.latitude locationInfo),
               PureStructs.ParamsText "longitude" ((T.pack . show) $ Telegram.longitude locationInfo)
             ]
