module API.Telegram.Cleaners where

import qualified API.Telegram.Structs as TStructs 
import qualified Logic.PureStructs as PureStructs 
import qualified Logger.Logger as Logger 

updResultToComMessage ::  TStructs.MessageInfo -> PureStructs.CMessage 
updResultToComMessage mInfo = case TStructs.sticker mInfo of 
    Just val -> PureStructs.Sticker  val 
    _ -> case TStructs.txt mInfo of 
        Just val -> PureStructs.Txt  val 
        _-> case TStructs.animation mInfo of 
            Just val -> PureStructs.Animation  val 
            _-> case TStructs.audio mInfo of 
                Just val -> PureStructs.Audio  val 
                _-> case TStructs.document mInfo of 
                    Just val -> PureStructs.Document  val 
                    _-> case TStructs.photo mInfo of 
                        Just val -> PureStructs.Photo  val 
                        _-> case TStructs.video mInfo of 
                            Just val -> PureStructs.Video  val
                            _-> case TStructs.voice mInfo of 
                                Just val -> PureStructs.Voice  val 
                                _-> case TStructs.contact mInfo of 
                                    Just val -> PureStructs.Contact  val 
                                    _-> case TStructs.poll mInfo of 
                                        Just val -> PureStructs.Poll  val 
                                        _-> case TStructs.venue mInfo of 
                                            Just val -> PureStructs.Venue  val 
                                            _-> case TStructs.location mInfo of 
                                                Just val -> PureStructs.Location  val 
                                                _-> PureStructs.Other

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
    in case msg of 
        PureStructs.Txt text -> case text of 
            "/help" -> pure (PureStructs.UserCommand uid $ 
                PureStructs.Command ((TStructs.chat_id . TStructs.chat) mInfo) text)
            "/repeat" -> pure (PureStructs.UserCommand uid $ 
                PureStructs.Command ((TStructs.chat_id . TStructs.chat) mInfo) text)
            _ -> pure $ 
                PureStructs.CommonMessage uid 
                    ((TStructs.chat_id . TStructs.chat) mInfo) 
                    (PureStructs.Txt text) 
                    (TStructs.caption mInfo)
        _ -> pure $ PureStructs.CommonMessage uid ((TStructs.chat_id . TStructs.chat) mInfo) msg (TStructs.caption mInfo)

updatesToPureMessageList :: Either Logger.LogMessage TStructs.TelegramUpdates 
    -> IO (Either Logger.LogMessage [PureStructs.Message])
updatesToPureMessageList (Left err) = return $ Left err
updatesToPureMessageList (Right tup) = do
    msgs <- mapM updResultToMessage (TStructs.result tup)
    return $ Right msgs 
    