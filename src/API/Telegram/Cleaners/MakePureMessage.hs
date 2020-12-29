module API.Telegram.Cleaners.MakePureMessage where

import Control.Applicative ( Alternative((<|>)) )
import qualified API.Telegram.Structs as TStructs 
import qualified Logic.PureStructs as PureStructs 
import qualified Logger.Logger as Logger 
import qualified Config.Config as Config 
import qualified Logger.LoggerMsgs as LoggerMsgs 
import API.Telegram.Cleaners.MbMsgType
    ( mbVoice,
      mbPhoto,
      mbContact,
      mbVenue,
      mbLocation,
      mbSticker,
      mbTextMessage,
      mbAnimation,
      mbAudio,
      mbPoll,
      mbDoc,
      mbVideo )


telUpdateToPureMessage :: Config.Config -> TStructs.TelUpdateResult 
    -> Either Logger.LogMessage PureStructs.PureMessage
telUpdateToPureMessage config res = do 
    let uid = TStructs.update_id res 
    let mbPureMessage = mbMakeCallbackPureMessage res uid <|> mbMakePureMessage config res uid 
    case mbPureMessage of 
        Just msg -> pure msg 
        Nothing -> Left LoggerMsgs.noUpd 
    
mbMakeCallbackPureMessage :: TStructs.TelUpdateResult 
    -> PureStructs.UpdateID
    -> Maybe PureStructs.PureMessage
mbMakeCallbackPureMessage res uid = case TStructs.callback_query res of 
    Just (TStructs.Callback callback cbData) -> do 
        let chid = (TStructs.cb_chid . TStructs.cb_chat) callback
        pure (PureStructs.PureMessage 
            (PureStructs.MTCallbackQuery cbData)
            uid 
            (Just chid)
            (Just $ [PureStructs.ParamsText "text" (PureStructs.newRepeatText (PureStructs.getNewRep cbData))
                , PureStructs.ParamsNum "chat_id" chid]))
    _ -> Nothing

mbMakePureMessage :: Config.Config
    ->  TStructs.TelUpdateResult 
    -> PureStructs.UpdateID
    -> Maybe PureStructs.PureMessage
mbMakePureMessage config res uid = case TStructs.messageInfo res of 
    Nothing -> pure $ PureStructs.PureMessage PureStructs.MTEmpty uid Nothing Nothing 
    Just mInfo -> do 
        let chid = TStructs.chat_id $ TStructs.chat mInfo 
        case makeCommonMessage config uid chid mInfo of 
            Nothing -> Nothing 
            Just pureMsg -> pure pureMsg 

makeCommonMessage :: Config.Config 
    -> PureStructs.UpdateID 
    -> PureStructs.ChatID 
    -> TStructs.MessageInfo 
    -> Maybe PureStructs.PureMessage
makeCommonMessage config uid chid mInfo = mbAnimation uid chid mInfo 
    <|> mbAudio uid chid mInfo 
    <|> mbDoc uid chid mInfo 
    <|> mbVideo uid chid mInfo 
    <|> mbVoice uid chid mInfo
    <|> mbPhoto uid chid mInfo
    <|> mbContact uid chid mInfo 
    <|> mbVenue uid chid mInfo
    <|> mbLocation uid chid mInfo    
    <|> mbSticker uid chid mInfo
    <|> mbPoll uid chid mInfo
    <|> mbTextMessage config uid chid mInfo