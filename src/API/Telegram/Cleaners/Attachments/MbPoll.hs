module API.Telegram.Cleaners.Attachments.MbPoll where

import API.Telegram.Cleaners.GetParams (basicParams)
import API.Telegram.Cleaners.MakeMbParams
  ( makeMaybeBoolParams,
    makeMaybeNumParams,
    makeMaybeTextParams,
  )
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

mbPoll ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
mbPoll updateId chatId msgInfo = do
  pollInfo <- TStructs.poll msgInfo
  mbPoll' updateId chatId msgInfo pollInfo

mbPoll' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelPoll ->
  Maybe PureStructs.PureMessage
mbPoll' updateId chatId msgInfo pollInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Poll")
      updateId
      (Just chatId)
      (pollParams chatId msgInfo pollInfo)

pollParams :: PureStructs.ChatID -> TStructs.MessageInfo -> TStructs.TelPoll -> Maybe [PureStructs.Params]
pollParams chatId msgInfo pollInfo =
  Just $
    basicParams chatId msgInfo
      <> [ PureStructs.ParamsText "question" (TStructs.question pollInfo),
           PureStructs.ParamsTextList "options" (map TStructs.poll_option (TStructs.poll_options pollInfo))
         ]
      <> makeMaybeBoolParams "is_anonimous" (TStructs.is_anonymous pollInfo)
      <> makeMaybeBoolParams "allows_multiple_answers" (TStructs.allows_multiple_answers pollInfo)
      <> makeMaybeNumParams "correct_option_id" (TStructs.correct_option_id pollInfo)
      <> makeMaybeTextParams "explanation" (TStructs.explanation pollInfo)
      <> makeMaybeNumParams "open_period" (TStructs.open_period pollInfo)
      <> makeMaybeNumParams "close_date" (TStructs.close_date pollInfo)
      <> makeMaybeBoolParams "is_closed" (TStructs.is_closed pollInfo)
