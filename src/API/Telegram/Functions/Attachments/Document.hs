module API.Telegram.Functions.Attachments.Document where

import API.Telegram.Functions.Params (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.Structs as PureStructs

buildDocumentMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
buildDocumentMessage updateId chatiId msgInfo = do
  docInfo <- TStructs.document msgInfo
  buildDocumentMessage' updateId chatiId msgInfo docInfo

buildDocumentMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelDocument ->
  Maybe PureStructs.PureMessage
buildDocumentMessage' updateId chatiId msgInfo docInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Document")
      updateId
      (Just chatiId)
      documentParams
  where
    documentParams =
      Just $
        basicParams chatiId msgInfo
          <> [PureStructs.ParamsText "document" (TStructs.document_file_id docInfo)]
