module API.Telegram.Cleaners.Attachments.MbLocation where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

mbLocation ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbLocation updateId chatId msgInfo = do
  locationInfo <- TStructs.location msgInfo
  mbLocation' updateId chatId msgInfo locationInfo

mbLocation' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelLocation ->
  Maybe PureStructs.PureMessage
mbLocation' updateId chatId msgInfo locationInfo =
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
