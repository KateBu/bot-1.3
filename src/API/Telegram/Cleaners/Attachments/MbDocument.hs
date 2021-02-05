module API.Telegram.Cleaners.Attachments.MbDocument where

import API.Telegram.Cleaners.GetParams (basicParams)
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbDocument ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbDocument updateId chatiId msgInfo = do
  docInfo <- TStructs.document msgInfo
  mbDocument' updateId chatiId msgInfo docInfo

mbDocument' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelDocument ->
  Maybe PureStructs.PureMessage
mbDocument' updateId chatiId msgInfo docInfo =
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
