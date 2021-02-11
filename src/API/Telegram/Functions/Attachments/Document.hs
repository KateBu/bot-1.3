module API.Telegram.Functions.Attachments.Document (buildDocumentMessage) where

import API.Telegram.Functions.Params (basicParams)
import qualified API.Telegram.Structs.MessageInfo as Telegram
import qualified Logic.Structs as PureStructs

buildDocumentMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Maybe PureStructs.PureMessage
buildDocumentMessage updateId chatiId msgInfo = do
  docInfo <- Telegram.document msgInfo
  buildDocumentMessage' updateId chatiId msgInfo docInfo

buildDocumentMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Telegram.Document ->
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
          <> [PureStructs.ParamsText "document" (Telegram.document_file_id docInfo)]
