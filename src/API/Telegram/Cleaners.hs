module API.Telegram.Cleaners where

import qualified Data.Text as T 
import qualified Data.ByteString.Lazy as BSL 
import Data.Aeson
    ( decode,
      eitherDecode,
      object,
      Value,
      KeyValue((.=)),
      ToJSON(toJSON) )
import Control.Applicative ( Alternative((<|>)) )
import qualified API.Telegram.Structs as TStructs 
import qualified Logic.PureStructs as PureStructs 
import qualified Logger.Logger as Logger 
import qualified Config.Config as Config 
import qualified Logger.LoggerMsgs as LoggerMsgs 


telByteStringToPureMessageList :: Config.Config -> Logger.Logger
    -> Either Logger.LogMessage BSL.ByteString 
    -> IO (Either Logger.LogMessage [PureStructs.PureMessage])
telByteStringToPureMessageList config logger eiBS = decodeByteString logger eiBS >>= telUpdatesToPureMessageList config 

decodeByteString :: Logger.Logger
    -> Either Logger.LogMessage BSL.ByteString     
    -> IO (Either Logger.LogMessage TStructs.TelegramUpdates)
decodeByteString _ (Left err) = pure $ Left err
decodeByteString logger (Right json) = do 
    Logger.botLog logger LoggerMsgs.getTelUpdScs
    case (decode json :: Maybe TStructs.TelegramUpdates) of 
        Just val -> 
                return $ Right val
        Nothing -> case (eitherDecode json :: Either String TStructs.TelegramUpdatesError) of 
            Right val -> return $ Left 
                (Logger.makeLogMessage LoggerMsgs.getUpdFld 
                    ("\n\terror code: " <> (T.pack . show . TStructs.error_code) val 
                    <> "\n\terror describtion: " <> TStructs.description val))
            Left msg -> return $ Left (Logger.makeLogMessage LoggerMsgs.getUpdFld (T.pack msg))

telUpdatesToPureMessageList :: Config.Config ->  Either Logger.LogMessage TStructs.TelegramUpdates 
    -> IO (Either Logger.LogMessage [PureStructs.PureMessage])
telUpdatesToPureMessageList _ (Left err) = pure $ Left err 
telUpdatesToPureMessageList config (Right tUpd) = pure $ mapM (telUpdateToPureMessage config) (TStructs.result tUpd)

telUpdateToPureMessage :: Config.Config -> TStructs.TelUpdateResult 
    -> Either Logger.LogMessage PureStructs.PureMessage
telUpdateToPureMessage config res = do 
    let uid = TStructs.update_id res 
    let mbPureMessage = mbMakeCallbackPureMessage res uid <|> mbGetMessageInfo config res uid 
    case mbPureMessage of 
        Just msg -> pure msg 
        Nothing -> Left LoggerMsgs.noUpd 
    
mbMakeCallbackPureMessage :: TStructs.TelUpdateResult 
    -> PureStructs.UpdateID
    -> Maybe PureStructs.PureMessage
mbMakeCallbackPureMessage res uid = case TStructs.callback_query res of 
    Just (TStructs.Callback callback cbData) -> pure (PureStructs.PureMessage 
        (PureStructs.MTCallbackQuery cbData)
            uid 
            (Just $ (TStructs.cb_chid . TStructs.cb_chat) callback)
            Nothing)
    _ -> Nothing

mbGetMessageInfo :: Config.Config
    ->  TStructs.TelUpdateResult 
    -> PureStructs.UpdateID
    -> Maybe PureStructs.PureMessage
mbGetMessageInfo config res uid = case TStructs.messageInfo res of 
    Nothing -> pure $ PureStructs.PureMessage PureStructs.MTEmpty uid Nothing Nothing 
    Just mInfo -> do 
        let chid = TStructs.chat_id $ TStructs.chat mInfo 
        case makePureMessage config uid chid mInfo of 
            Nothing -> Nothing 
            Just pureMsg -> pure pureMsg 

makePureMessage :: Config.Config 
    -> PureStructs.UpdateID 
    -> PureStructs.ChatID 
    -> TStructs.MessageInfo 
    -> Maybe PureStructs.PureMessage
makePureMessage config uid chid mInfo = mbAnimation uid chid mInfo 
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

mbAnimation, mbAudio, mbDoc, mbVideo
    , mbVoice, mbPhoto, mbContact, mbVenue 
    , mbLocation, mbSticker, mbPoll :: PureStructs.UpdateID 
    -> PureStructs.ChatID 
    -> TStructs.MessageInfo 
    -> Maybe PureStructs.PureMessage      

mbTextMessage :: Config.Config -> PureStructs.UpdateID 
    -> PureStructs.ChatID 
    -> TStructs.MessageInfo 
    -> Maybe PureStructs.PureMessage  
mbTextMessage config uid chid mInfo = case TStructs.txt mInfo of 
    Just text -> case text of 
        "/help" -> pure $ PureStructs.PureMessage
            (PureStructs.MTUserCommand PureStructs.Help)
            uid 
            (Just chid)
            (Just $ basicParams chid mInfo 
                <> [PureStructs.ParamsText "text" (Config.helpMessage config)])   
        "/repeat" -> pure $ PureStructs.PureMessage 
            (PureStructs.MTUserCommand PureStructs.Repeat)
            uid 
            (Just chid)
            (Just $ basicParams chid mInfo 
                <> [PureStructs.ParamsJSON "reply_markup" (makeKeyboard ((map (map TButtons)) PureStructs.buttons'))
                , PureStructs.ParamsBool "one_time_keyboard" True])

        _ -> pure $ PureStructs.PureMessage 
            (PureStructs.MTCommon "Message")
            uid 
            (Just chid)
            (Just $ basicParams chid mInfo 
                <> [PureStructs.ParamsText "text" text])
    Nothing -> Nothing 

mbAnimation uid chid mInfo = case TStructs.animation mInfo of 
    Just anim -> pure $ PureStructs.PureMessage 
        (PureStructs.MTCommon "Animation")
        uid
        (Just chid)
        (Just $ basicParams chid mInfo 
            <> [PureStructs.ParamsText "animation" (TStructs.animation_file_id anim)])
    Nothing -> Nothing 

mbAudio uid chid mInfo = case TStructs.audio mInfo of 
    Just aud -> pure $ PureStructs.PureMessage
        (PureStructs.MTCommon "Audio")
        uid 
        (Just chid)
        (Just $ basicParams chid mInfo 
            <> [PureStructs.ParamsText "audio" (TStructs.audio_id aud)])
    Nothing -> Nothing 

mbDoc uid chid mInfo = case TStructs.document mInfo of
    Just doc -> pure $ PureStructs.PureMessage 
        (PureStructs.MTCommon "Document")
        uid
        (Just chid)
        (Just $ basicParams chid mInfo 
            <> [PureStructs.ParamsText "document" (TStructs.doc_file_id doc)])
    Nothing -> Nothing 

mbVideo uid chid mInfo = case TStructs.video mInfo of
    Just video -> pure $ PureStructs.PureMessage 
        (PureStructs.MTCommon "Video")
        uid
        (Just chid)
        (Just $ basicParams chid mInfo 
            <> [PureStructs.ParamsText "video" (TStructs.video_file_id video)])
    Nothing -> Nothing 

mbVoice uid chid mInfo = case TStructs.voice mInfo of
    Just voice -> pure $ PureStructs.PureMessage 
        (PureStructs.MTCommon "Voice")
        uid 
        (Just chid)
        (Just $ basicParams chid mInfo 
            <> [PureStructs.ParamsText "voice" (TStructs.voice_file_id voice)])
    Nothing -> Nothing

mbPhoto uid chid mInfo = case TStructs.photo mInfo of 
    Just photo -> pure $ PureStructs.PureMessage 
        (PureStructs.MTCommon "Photo")
        uid 
        (Just chid)
        (Just $ basicParams chid mInfo 
        <> getPhotoParams photo)    
    Nothing -> Nothing 

mbContact uid chid mInfo = case TStructs.contact mInfo of 
    Just contact -> pure $ PureStructs.PureMessage 
        (PureStructs.MTCommon "Contact")
        uid 
        (Just chid)
        (Just $ basicParams chid mInfo 
        <> [PureStructs.ParamsText "phone_number" (TStructs.phone_number contact)
            , PureStructs.ParamsText "first_name" (TStructs.first_name contact)            
            ] 
        <> makeMaybeTextParams "last_name" (TStructs.last_name contact)
        <> makeMaybeTextParams "vcard" (TStructs.vcard contact)
        )
    Nothing -> Nothing

mbVenue uid chid mInfo = case TStructs.venue mInfo of 
    Just venue -> pure $ PureStructs.PureMessage 
        (PureStructs.MTCommon "Venue")
        uid 
        (Just chid)
        (Just $ basicParams chid mInfo 
        <> [PureStructs.ParamsText "latitude" ((T.pack . show) $ TStructs.v_latitude venue)
            , PureStructs.ParamsText "longitude" ((T.pack . show) $ TStructs.v_longitude venue)
            , PureStructs.ParamsText "title" (TStructs.v_title venue)      
            , PureStructs.ParamsText "address" (TStructs.v_address venue)      
            ] )
    Nothing -> Nothing

mbLocation uid chid mInfo = case TStructs.location mInfo of 
    Just location -> pure $ PureStructs.PureMessage 
        (PureStructs.MTCommon "Location")
        uid 
        (Just chid)
        (Just $ basicParams chid mInfo 
        <> [PureStructs.ParamsText "latitude" ((T.pack . show) $ TStructs.latitude location)
            , PureStructs.ParamsText "longitude" ((T.pack . show) $ TStructs.longitude location) 
        ])
    Nothing -> Nothing

mbSticker uid chid mInfo = case TStructs.sticker mInfo of 
    Just sticker -> pure $ PureStructs.PureMessage 
        (PureStructs.MTCommon "Sticker")
        uid 
        (Just chid)
        (Just $ basicParams chid mInfo 
        <> [PureStructs.ParamsText "sticker" (TStructs.s_file_id sticker)
        ])
    Nothing -> Nothing 

mbPoll uid chid mInfo = case TStructs.poll mInfo of 
    Just poll -> pure $ PureStructs.PureMessage 
        (PureStructs.MTCommon "Poll")
        uid 
        (Just chid)
        (Just $ basicParams chid mInfo 
        <> [PureStructs.ParamsText "question" (TStructs.question poll)        
        , PureStructs.ParamsTextList "options" (map TStructs.poll_option (TStructs.poll_options poll))]
        <> makeMaybeBoolParams "is_anonimous" (TStructs.is_anonymous poll)
    --    <> makeMaybeTextParams "type" (TStructs.poll_type poll)
        <> makeMaybeBoolParams "allows_multiple_answers" (TStructs.allows_multiple_answers poll)
        <> makeMaybeNumParams "correct_option_id" (TStructs.correct_option_id poll)
        <> makeMaybeTextParams "explanation" (TStructs.explanation poll)
        <> makeMaybeNumParams "open_period" (TStructs.open_period poll)
        <> makeMaybeNumParams "close_date" (TStructs.close_date poll)
        <> makeMaybeBoolParams "is_closed" (TStructs.is_closed poll)
        )
    Nothing -> Nothing

getPhotoParams :: [TStructs.TelPhoto] -> [PureStructs.Params]
getPhotoParams [] = []
getPhotoParams (x:_) = [PureStructs.ParamsText "photo" (TStructs.photo_file_id x)]

mbCaption :: TStructs.MessageInfo -> [PureStructs.Params]
mbCaption mInfo = case TStructs.caption mInfo of 
    Just caption -> pure $ PureStructs.ParamsText "caption" caption 
    Nothing -> [] 

basicParams :: PureStructs.ChatID -> TStructs.MessageInfo -> [PureStructs.Params] 
basicParams chid mInfo = PureStructs.ParamsNum "chat_id" chid : mbCaption mInfo 

makeMaybeTextParams ::  T.Text -> Maybe T.Text -> [PureStructs.Params]
makeMaybeTextParams _ Nothing = [] 
makeMaybeTextParams key (Just val) = pure $ PureStructs.ParamsText key val

makeMaybeBoolParams :: T.Text -> Maybe Bool -> [PureStructs.Params]
makeMaybeBoolParams _ Nothing = []
makeMaybeBoolParams key (Just val) = pure $ PureStructs.ParamsBool key val

makeMaybeNumParams :: T.Text -> Maybe Int -> [PureStructs.Params]
makeMaybeNumParams _ Nothing = []
makeMaybeNumParams key (Just val) = pure $ PureStructs.ParamsNum key val

newtype TButtons = TButtons PureStructs.PureButtons 

instance ToJSON TButtons where 
    toJSON (TButtons (PureStructs.PureButtons lbl txt)) = object ["text" .= lbl, "callback_data" .= txt] 

makeKeyboard :: [[TButtons]] -> Value
makeKeyboard btns = object ["inline_keyboard" .= btns]
