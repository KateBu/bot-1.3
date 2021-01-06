module API.Telegram.Cleaners.MbDoc where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.TStructs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbDoc ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbDoc uid chid mInfo =
  let mbDocument = TStructs.document mInfo
   in maybe Nothing (mbDoc' uid chid mInfo) mbDocument

mbDoc' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelDocument ->
  Maybe PureStructs.PureMessage
mbDoc' uid chid mInfo doc =
  pure $
    PureStructs.PureMessage
      (PureStructs.MTCommon "Document")
      uid
      (Just chid)
      ( Just $
          basicParams chid mInfo
            <> [PureStructs.ParamsText "document" (TStructs.doc_file_id doc)]
      )
