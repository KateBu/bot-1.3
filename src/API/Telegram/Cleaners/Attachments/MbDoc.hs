module API.Telegram.Cleaners.Attachments.MbDoc where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbDoc ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbDoc uid chid mInfo = do
  docInfo <- TStructs.document mInfo
  mbDoc' uid chid mInfo docInfo

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
      docParams
  where
    docParams =
      Just $
        basicParams chid mInfo
          <> [PureStructs.ParamsText "document" (TStructs.doc_file_id doc)]
