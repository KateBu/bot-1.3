module API.Telegram.Cleaners where

import qualified Data.Text as T 
import qualified Data.ByteString.Lazy as BSL 
import Data.Aeson 
import Control.Applicative

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
decodeByteString logger eiJs = case eiJs of 
    Left err -> pure $ Left err 
    Right js -> do 
        Logger.botLog logger LoggerMsgs.getTelUpdScs
        case (decode js :: Maybe TStructs.TelegramUpdates) of 
            Just val -> 
                    return $ Right val
            Nothing -> case (eitherDecode js :: Either String TStructs.TelegramUpdatesError) of 
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
    -> (Either Logger.LogMessage PureStructs.PureMessage)
--telUpdateToPureMessage _ (Left err) = pure $ Left err 
telUpdateToPureMessage config res = do 
    let uid = TStructs.update_id res 
    case TStructs.callback_query res of 
        Just (TStructs.Callback _ cbData) -> Right (PureStructs.PureMessage 
            (PureStructs.MTCallbackQuery cbData)
            uid 
            Nothing
            Nothing)
        Nothing -> do 
            case TStructs.messageInfo res of 
                Nothing -> Left LoggerMsgs.noUpd 
                Just mInfo -> do 
                    let chid = TStructs.chat_id $ TStructs.chat mInfo 
                    case makePureMessage uid chid mInfo of 
                        Nothing -> Left LoggerMsgs.noUpd 
                        Just pureMsg -> Right pureMsg 

makePureMessage :: PureStructs.UpdateID -> PureStructs.ChatID -> TStructs.MessageInfo -> Maybe PureStructs.PureMessage
makePureMessage uid chid mInfo = mbAnimation uid chid mInfo 
    <|> mbTextMessage uid chid mInfo



mbTextMessage, mbAnimation, mbAudio :: PureStructs.UpdateID 
    -> PureStructs.ChatID 
    -> TStructs.MessageInfo 
    -> Maybe PureStructs.PureMessage             

mbTextMessage uid chid mInfo = case TStructs.txt mInfo of 
    Just text -> pure $ PureStructs.PureMessage 
        (PureStructs.MTCommon "Message")
        uid 
        (Just chid)
        (Just [PureStructs.ParamsText "text" text
            , PureStructs.ParamsNum "chat_id" chid
            ])
    Nothing -> Nothing 

mbAnimation uid chid mInfo = case TStructs.animation mInfo of 
    Just anim -> pure $ PureStructs.PureMessage 
        (PureStructs.MTCommon "Animation")
        uid
        (Just chid)
        (Just [PureStructs.ParamsText "animation" (TStructs.animation_file_id anim)
            , PureStructs.ParamsNum "chat_id" chid
            ])
    Nothing -> Nothing 

mbAudio uid chid mInfo = case TStructs.audio mInfo of 
    Just aud -> pure $ PureStructs.PureMessage
        (PureStructs.MTCommon "Audio")
        uid 
        (Just chid)
        (Just [PureStructs.ParamsText "audio" (TStructs.audio_id aud)
            , PureStructs.ParamsNum "chat_id" chid
            ])


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