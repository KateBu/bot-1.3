module API.Telegram.Cleaners where

import qualified Data.Text as T 

import API.Telegram.Structs
import Logic.PureStructs
import Logger.Logger
import Logger.LoggerMsgs

type Err = T.Text  



updResultToComMessage ::  MessageInfo -> CMessage 
updResultToComMessage mInfo = case sticker mInfo of 
    Just val -> Sticker  val 
    _ -> case txt mInfo of 
        Just val -> Txt  val 
        _-> case animation mInfo of 
            Just val -> Animation  val 
            _-> case audio mInfo of 
                Just val -> Audio  val 
                _-> case document mInfo of 
                    Just val -> Document  val 
                    _-> case photo mInfo of 
                        Just val -> Photo  val 
                        _-> case video mInfo of 
                            Just val -> Video  val
                            _-> case voice mInfo of 
                                Just val -> Voice  val 
                                _-> case contact mInfo of 
                                    Just val -> Contact  val 
                                    _-> case poll mInfo of 
                                        Just val -> Poll  val 
                                        _-> case venue mInfo of 
                                            Just val -> Venue  val 
                                            _-> case location mInfo of 
                                                Just val -> Location  val 
                                                _-> Other


updResultToMessage :: TelUpdateResult -> IO Message
updResultToMessage (TelUpdateResult uid _ (Just callback)) = 
    pure $ CallbackQuery uid ((cb_chid . cb_chat . cb_msg) callback) (cb_data callback)

updResultToMessage (TelUpdateResult uid Nothing _) = 
    pure $ EmptyMessage uid 

updResultToMessage (TelUpdateResult uid (Just mInfo) _) = 
    let msg = updResultToComMessage mInfo 
    in case msg of 
        Txt text -> case text of 
            "/help" -> pure (UserCommand uid $ Command ((chat_id . chat) mInfo) text)
            "/repeat" -> pure (UserCommand uid $ Command ((chat_id . chat) mInfo) text)
            _ -> pure $ CommonMessage uid ((chat_id . chat) mInfo) (Txt text) (caption mInfo)
        _ -> pure $ CommonMessage uid ((chat_id . chat) mInfo) msg (caption mInfo)


updatesToPureMessageList :: Either LogMessage TelegramUpdates -> IO (Either LogMessage [Message])
updatesToPureMessageList (Left err) = return $ Left err
updatesToPureMessageList (Right tup) = do
    msgs <- mapM updResultToMessage (result tup)
    return $ Right msgs 
    