module API.Telegram.Functions.Attachments.Poll where

import API.Telegram.Functions.BasicParams (basicParams)
import API.Telegram.Functions.Params
  ( buildBoolParams,
    buildNumParams,
    buildTextParams,
  )
import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.Structs as PureStructs

buildPollMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
buildPollMessage updateId chatId msgInfo = do
  pollInfo <- TStructs.poll msgInfo
  buildPollMessage' updateId chatId msgInfo pollInfo

buildPollMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  TStructs.TelPoll ->
  Maybe PureStructs.PureMessage
buildPollMessage' updateId chatId msgInfo pollInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Poll")
      updateId
      (Just chatId)
      (buildPollParams chatId msgInfo pollInfo)

buildPollParams :: PureStructs.ChatID -> TStructs.MessageInfo -> TStructs.TelPoll -> Maybe [PureStructs.Params]
buildPollParams chatId msgInfo pollInfo =
  Just $
    basicParams chatId msgInfo
      <> [ PureStructs.ParamsText "question" (TStructs.question pollInfo),
           PureStructs.ParamsTextList "options" (map TStructs.poll_option (TStructs.poll_options pollInfo))
         ]
      <> buildBoolParams "is_anonimous" (TStructs.is_anonymous pollInfo)
      <> buildBoolParams "allows_multiple_answers" (TStructs.allows_multiple_answers pollInfo)
      <> buildNumParams "correct_option_id" (TStructs.correct_option_id pollInfo)
      <> buildTextParams "explanation" (TStructs.explanation pollInfo)
      <> buildNumParams "open_period" (TStructs.open_period pollInfo)
      <> buildNumParams "close_date" (TStructs.close_date pollInfo)
      <> buildBoolParams "is_closed" (TStructs.is_closed pollInfo)
