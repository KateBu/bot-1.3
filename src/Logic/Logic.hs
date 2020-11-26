module Logic.Logic where

import qualified Data.Text as T

import Handle.Handle 
import Logic.PureStructs
import Logger.Logger  
import Logger.LoggerMsgs
import Config.Config
import qualified Data.Map as Map 

processMessages :: (Monad m) => Config 
    -> [Message] 
    -> (Config -> Message -> m (Either LogMessage Config))  
    -> m (Either LogMessage Config)
processMessages config msgs function = do 
    eiConfs <- mapM (processMessage_ config function) msgs 
    case eiConfs of 
        [] -> pure $ Left emptyList
        _ -> case last eiConfs of 
            Left err -> pure $ Left err
            Right config -> pure $ Right config 



processMessage_ :: (Monad m) => Config 
    -> (Config -> Message -> m (Either LogMessage Config)) 
    -> Message     
    -> m (Either LogMessage Config)
processMessage_ config _ (EmptyMessage uid)  = 
    pure $ Right (configSetOffset config (succ uid))

processMessage_ config sendFunction (UserCommand uid command)  = case text command of 
    "/help" -> sendFunction config 
        (CommonMessage uid (chatID command) (Txt (helpMessage config)) Nothing)
    "/repeat" -> sendFunction config 
        (CommonMessage uid (chatID command) (Buttons buttons) Nothing)
    _ -> pure $ Left cmdMsgFld

processMessage_ config sendFunction (CallbackQuery uid chid txt)  = do 
    let newConfig = setNewRepetition config chid txt 
    sendFunction newConfig 
        (CommonMessage uid chid (Txt (repeatText <> (T.pack . show) (getNewRepetition txt))) Nothing )

processMessage_ config sendFunction (CommonMessage uid chid cMsg mCap)  = 
    repeatMessage config (CommonMessage uid chid cMsg mCap)(findUserRepeat config chid) sendFunction 


repeatMessage :: (Monad m) => Config -> Message 
    -> Int
    -> (Config -> Message -> m (Either LogMessage Config))
    -> m (Either LogMessage Config)
repeatMessage config _ 0 _ = pure $ Right config 
repeatMessage config msg n function = do 
    eiConfig <- function config msg 
    case eiConfig of 
        Left err -> pure $ Left err 
        Right newConfig -> repeatMessage newConfig msg (n-1) function 


setNewRepetition :: Config -> Integer -> T.Text -> Config 
setNewRepetition config chid queryText = 
    setUserRepeat config chid (getNewRepetition queryText)




getNewRepetition :: T.Text -> Int 
getNewRepetition "/setRepetition1" = 1 
getNewRepetition "/setRepetition2" = 2 
getNewRepetition "/setRepetition3" = 3 
getNewRepetition "/setRepetition4" = 4 
getNewRepetition "/setRepetition5" = 5 



getResultInfo :: ProcessMessageResult -> T.Text
getResultInfo res = 
    "\n\tUpdate ID: "
    <> (T.pack . show . updID) res 
    <> "\n\tMessage Type: "
    <> msgType res 
    <> maybeChid (mbChatID res)
    <> maybeMsgInfo (mbMessage res)

maybeChid :: Maybe Integer -> T.Text
maybeChid Nothing = T.empty
maybeChid (Just chid) = (T.pack . show) chid 

maybeMsgInfo :: Maybe CMessage -> T.Text
maybeMsgInfo Nothing = T.empty
maybeMsgInfo (Just msg) = getMessageType msg

