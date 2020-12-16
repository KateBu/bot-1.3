module API.Telegram.Cleaners where

import qualified Data.Text as T 
import qualified Data.ByteString.Lazy as BSL 
import Data.Aeson ( decode, eitherDecode ) 
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
    Just (TStructs.Callback _ cbData) -> pure (PureStructs.PureMessage 
            (PureStructs.MTCallbackQuery cbData)
            uid 
            Nothing
            Nothing)
    _ -> Nothing

mbGetMessageInfo :: Config.Config
    ->  TStructs.TelUpdateResult 
    -> PureStructs.UpdateID
    -> Maybe PureStructs.PureMessage
mbGetMessageInfo config res uid = case TStructs.messageInfo res of 
    Nothing -> Nothing
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
--    <|> mbSticker uid chid mInfo
    <|> mbTextMessage config uid chid mInfo

mbAnimation, mbAudio, mbDoc, mbVideo
    , mbVoice, mbPhoto, mbContact, mbVenue 
    , mbLocation, mbSticker :: PureStructs.UpdateID 
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
        <> [PureStructs.ParamsText "sticker" ((T.pack . show) $ TStructs.s_file_id sticker)
        ])
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


-- The functions below will be removed soon 

{-
updResultToComMessage ::  TStructs.MessageInfo -> PureStructs.ComMessage 
updResultToComMessage mInfo = case TStructs.sticker mInfo of 
    Just val -> PureStructs.defaultComMsg {
            PureStructs.commonMsgType = "Sticker"
            , PureStructs.mbSticker = Just (PureStructs.PureSticker 
                (TStructs.s_file_id val) 
                (TStructs.s_is_animated val))
        }
    _ -> case TStructs.txt mInfo of 
        Just val -> PureStructs.defaultComMsg {
            PureStructs.commonMsgType = "Message"
            , PureStructs.mbText = Just val
        } 
        _-> case TStructs.animation mInfo of 
            Just val -> PureStructs.defaultComMsg {
                PureStructs.commonMsgType = "Animation"
                , PureStructs.mbAnimationFileId = Just (TStructs.animation_file_id val)
            }   
            _-> case TStructs.audio mInfo of 
                Just val -> PureStructs.defaultComMsg {
                    PureStructs.commonMsgType = "Audio"
                    , PureStructs.mbAudio = Just (PureStructs.PureAudio
                        (TStructs.audio_id val)
                        (TStructs.audio_duration val)
                        (TStructs.audio_performer val)
                        (TStructs.audio_title val))
                }  
                _-> case TStructs.document mInfo of 
                    Just val -> PureStructs.defaultComMsg {
                        PureStructs.commonMsgType = "Document"
                        , PureStructs.mbDocFileId = Just (TStructs.doc_file_id val)
                    } 
                    _-> case TStructs.photo mInfo of 
                        Just val -> PureStructs.defaultComMsg {
                            PureStructs.commonMsgType = "Photo"
                            , PureStructs.mbPhotoFileIds = Just (TStructs.photo_file_id <$> val)
                        } 
                        _-> case TStructs.video mInfo of 
                            Just val -> PureStructs.defaultComMsg {
                                PureStructs.commonMsgType = "Video"
                                , PureStructs.mbVideoFileId = Just (TStructs.video_file_id val)
                            }
                            _-> case TStructs.voice mInfo of 
                                Just val -> PureStructs.defaultComMsg { 
                                    PureStructs.commonMsgType = "Voice"
                                    , PureStructs.mbVoiceFileId = Just (TStructs.voice_file_id val)
                                } 
                                _-> case TStructs.contact mInfo of 
                                    Just val -> PureStructs.defaultComMsg {
                                        PureStructs.commonMsgType = "Contact"
                                        , PureStructs.mbContact = Just (PureStructs.PureContact 
                                            (TStructs.phone_number val)
                                            (TStructs.first_name val)
                                            (TStructs.last_name val)
                                            (TStructs.vcard val))
                                    } 
                                    _-> case TStructs.poll mInfo of 
                                        Just val -> PureStructs.defaultComMsg {
                                            PureStructs.commonMsgType = "Poll"
                                            , PureStructs.mbPoll = Just (PureStructs.PurePoll 
                                                (TStructs.question val)
                                                (pollOptionToPair <$> (TStructs.poll_options val))
                                                (TStructs.is_anonymous val)
                                                (TStructs.poll_type val)
                                                (TStructs.allows_multiple_answers val)
                                                (TStructs.correct_option_id val)
                                                (TStructs.explanation val)
                                                (TStructs.open_period val)
                                                (TStructs.close_date val)
                                                (TStructs.is_closed val))
                                        } 
                                        _-> case TStructs.venue mInfo of 
                                            Just val -> PureStructs.defaultComMsg {
                                                PureStructs.commonMsgType = "Venue"
                                                , PureStructs.mbVenue = Just (PureStructs.PureVenue
                                                    (TStructs.v_latitude val)
                                                    (TStructs.v_longitude val)
                                                    (TStructs.v_title val)
                                                    (TStructs.v_address val))
                                            } 
                                            _-> case TStructs.location mInfo of 
                                                Just val -> PureStructs.defaultComMsg {
                                                    PureStructs.commonMsgType = "Location"
                                                    , PureStructs.mbLocation = Just (PureStructs.PureLocation 
                                                        (TStructs.latitude val)
                                                        (TStructs.longitude val))
                                                } 
                                                _-> PureStructs.defaultComMsg {
                                                    PureStructs.commonMsgType = "Other"
                                                }

pollOptionToPair :: TStructs.PollOptions -> (T.Text,Int)
pollOptionToPair (TStructs.PollOptions txt num) = (txt,num)

updResultToMessage :: TStructs.TelUpdateResult -> IO PureStructs.Message
updResultToMessage (TStructs.TelUpdateResult uid _ (Just callback)) = 
    pure $ PureStructs.CallbackQuery 
        uid 
        ((TStructs.cb_chid . TStructs.cb_chat . TStructs.cb_msg) callback) 
        (TStructs.cb_data callback)
updResultToMessage (TStructs.TelUpdateResult uid Nothing _) = 
    pure $ PureStructs.EmptyMessage uid 
updResultToMessage (TStructs.TelUpdateResult uid (Just mInfo) _) = 
    let msg = updResultToComMessage mInfo 
    in case PureStructs.commonMsgType msg of 
        "Message" -> case PureStructs.mbText msg of 
            Just "/help" -> pure (PureStructs.UserCommand uid $ 
                PureStructs.Command ((TStructs.chat_id . TStructs.chat) mInfo) "/help")
            Just "/repeat" -> pure (PureStructs.UserCommand uid $ 
                PureStructs.Command ((TStructs.chat_id . TStructs.chat) mInfo) "/repeat")
            _ -> pure $ 
                PureStructs.CommonMessage uid 
                    ((TStructs.chat_id . TStructs.chat) mInfo) 
                    msg  
                    (TStructs.caption mInfo)                   
        _ -> pure $ PureStructs.CommonMessage uid ((TStructs.chat_id . TStructs.chat) mInfo) msg (TStructs.caption mInfo)

updatesToPureMessageList :: Either Logger.LogMessage TStructs.TelegramUpdates 
    -> IO (Either Logger.LogMessage [PureStructs.Message])
updatesToPureMessageList (Left err) = return $ Left err
updatesToPureMessageList (Right tup) = do
    msgs <- mapM updResultToMessage (TStructs.result tup)
    return $ Right msgs 
    -}