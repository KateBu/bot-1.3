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
mbLocation uid chid mInfo =
  TStructs.location mInfo >>= mbLocation' uid chid mInfo

mbLocation' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelLocation ->
  Maybe PureStructs.PureMessage
mbLocation' uid chid mInfo location =
  pure $
    PureStructs.PureMessage
      (PureStructs.MTCommon "Location")
      uid
      (Just chid)
      ( Just $
          basicParams chid mInfo
            <> [ PureStructs.ParamsText "latitude" ((T.pack . show) $ TStructs.latitude location),
                 PureStructs.ParamsText "longitude" ((T.pack . show) $ TStructs.longitude location)
               ]
      )
