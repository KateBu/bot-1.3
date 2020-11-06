module API.Telegram.Cleaners where

import API.Telegram.Structs
import Logic.PureStructs
import Logger.Logger



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


updResultToMessage :: Logger -> TelUpdateResult -> IO (Maybe Message) 
updResultToMessage _ (TelUpdateResult uid _ (Just callback)) = 
    pure $ Just $ CallbackQuery uid ((cb_chid . cb_chat . cb_msg) callback) (cb_data callback)

updResultToMessage _ (TelUpdateResult uid Nothing _) = 
    pure $ Just $ EmptyMessage uid 

updResultToMessage logger (TelUpdateResult uid (Just mInfo) _) = 
    let msg = updResultToComMessage mInfo 
    in case msg of 
        Other -> do 
            botLog logger (LogMessage Error "Unknown Message Type recieved")
            return Nothing 
        Txt text -> case text of 
            "/help" -> pure $ Just (UserCommand uid $ Command ((chat_id . chat) mInfo) text)
            "/repeat" -> pure $ Just (UserCommand uid $ Command ((chat_id . chat) mInfo) text)
            _ -> pure $ Just $ CommonMessage uid ((chat_id . chat) mInfo) (Txt text) (caption mInfo)
        _ -> pure $ Just $ CommonMessage uid ((chat_id . chat) mInfo) msg (caption mInfo)


updatesToPureMessageList :: Logger -> Updates -> IO [Maybe Message]
updatesToPureMessageList logger updates = case updates of 
    TUpdates tup -> do 
        msgs <- mapM (updResultToMessage logger) (result tup)
        botLog logger (LogMessage Debug "Telegram Updates successfully converted to [Message]")
        return msgs 
    VKUpdates -> do 
        botLog logger (LogMessage Error "VK API is not implemented, cannot convert VK Updates to [Message]")
        return [] 