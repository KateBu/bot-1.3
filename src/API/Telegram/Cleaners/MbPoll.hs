module API.Telegram.Cleaners.MbPoll where

import qualified API.Telegram.TStructs.MessageInfo as TStructs 
import qualified Logic.PureStructs as PureStructs 
import API.Telegram.Cleaners.GetParams ( basicParams ) 
import API.Telegram.Cleaners.MakeMbParams
    ( makeMaybeBoolParams, makeMaybeNumParams, makeMaybeTextParams )

mbPoll :: PureStructs.UpdateID -> PureStructs.ChatID -> TStructs.MessageInfo 
    -> Maybe PureStructs.PureMessage   
mbPoll uid chid mInfo = 
    let mbPoll = TStructs.poll mInfo
    in maybe Nothing (mbPoll' uid chid mInfo) mbPoll
    
mbPoll' :: PureStructs.UpdateID -> PureStructs.ChatID -> TStructs.MessageInfo 
    -> TStructs.TelPoll -> Maybe PureStructs.PureMessage   
mbPoll' uid chid mInfo poll = pure $ PureStructs.PureMessage 
    (PureStructs.MTCommon "Poll")
    uid 
    (Just chid)
    (Just $ basicParams chid mInfo 
        <> [PureStructs.ParamsText "question" (TStructs.question poll)        
        , PureStructs.ParamsTextList "options" (map TStructs.poll_option (TStructs.poll_options poll))]
        <> makeMaybeBoolParams "is_anonimous" (TStructs.is_anonymous poll)
        <> makeMaybeBoolParams "allows_multiple_answers" (TStructs.allows_multiple_answers poll)
        <> makeMaybeNumParams "correct_option_id" (TStructs.correct_option_id poll)
        <> makeMaybeTextParams "explanation" (TStructs.explanation poll)
        <> makeMaybeNumParams "open_period" (TStructs.open_period poll)
        <> makeMaybeNumParams "close_date" (TStructs.close_date poll)
        <> makeMaybeBoolParams "is_closed" (TStructs.is_closed poll)
        )