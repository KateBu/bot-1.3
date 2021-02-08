module API.Telegram.Functions.Attachments.Location where

import API.Telegram.Functions.Params (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Data.Text as T
import qualified Logic.Structs as PureStructs

buildLocationMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
buildLocationMessage updateId chatId msgInfo = do
  locationInfo <- TStructs.location msgInfo
  buildLocationMessage' updateId chatId msgInfo locationInfo

buildLocationMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelLocation ->
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
        basicParams chatId msgInfo
          <> [ PureStructs.ParamsText "latitude" ((T.pack . show) $ TStructs.latitude locationInfo),
               PureStructs.ParamsText "longitude" ((T.pack . show) $ TStructs.longitude locationInfo)
             ]
