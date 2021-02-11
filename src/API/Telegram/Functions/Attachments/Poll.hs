module API.Telegram.Functions.Attachments.Poll (buildPollMessage) where

import API.Telegram.Functions.Params
  ( basicParams,
    buildBoolParams,
    buildNumParams,
    buildTextParams,
  )
import qualified API.Telegram.Structs.MessageInfo as Telegram
import qualified Logic.Structs as PureStructs

buildPollMessage ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Maybe PureStructs.PureMessage
buildPollMessage updateId chatId msgInfo = do
  pollInfo <- Telegram.poll msgInfo
  buildPollMessage' updateId chatId msgInfo pollInfo

buildPollMessage' ::
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  Telegram.MessageInfo ->
  Telegram.Poll ->
  Maybe PureStructs.PureMessage
buildPollMessage' updateId chatId msgInfo pollInfo =
  pure $
    PureStructs.PureMessage
      (PureStructs.MsgTypeCommon "Poll")
      updateId
      (Just chatId)
      (buildPollParams chatId msgInfo pollInfo)

buildPollParams :: PureStructs.ChatID -> Telegram.MessageInfo -> Telegram.Poll -> Maybe [PureStructs.Params]
buildPollParams chatId msgInfo pollInfo =
  Just $
    basicParams chatId msgInfo
      <> [ PureStructs.ParamsText "question" (Telegram.question pollInfo),
           PureStructs.ParamsTextList "options" (map Telegram.poll_option (Telegram.poll_options pollInfo))
         ]
      <> buildBoolParams "is_anonimous" (Telegram.is_anonymous pollInfo)
      <> buildBoolParams "allows_multiple_answers" (Telegram.allows_multiple_answers pollInfo)
      <> buildNumParams "correct_option_id" (Telegram.correct_option_id pollInfo)
      <> buildTextParams "explanation" (Telegram.explanation pollInfo)
      <> buildNumParams "open_period" (Telegram.open_period pollInfo)
      <> buildNumParams "close_date" (Telegram.close_date pollInfo)
      <> buildBoolParams "is_closed" (Telegram.is_closed pollInfo)
