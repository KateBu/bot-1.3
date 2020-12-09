module Logic.Logic where

import qualified Data.Text as T

import qualified Logic.PureStructs as PureStructs 
import qualified Logger.Logger as Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Config.Config as Config 


processMessages :: (Monad m) => Config.Config 
    -> [PureStructs.Message] 
    -> (Config.Config -> PureStructs.Message -> m (Either Logger.LogMessage Config.Config))  
    -> m (Either Logger.LogMessage Config.Config)
processMessages config msgs function = do 
    eiConfs <- mapM (processMessage_ config function) msgs 
    case eiConfs of 
        [] -> pure $ Left LoggerMsgs.emptyList
        _ -> case last eiConfs of 
            Left err -> pure $ Left err
            Right config -> pure $ Right config 

processMessage_ :: (Monad m) => Config.Config 
    -> (Config.Config -> PureStructs.Message -> m (Either Logger.LogMessage Config.Config)) 
    -> PureStructs.Message     
    -> m (Either Logger.LogMessage Config.Config)
processMessage_ config _ (PureStructs.EmptyMessage uid)  = 
    pure $ Right (Config.configSetOffset config (succ uid))
processMessage_ config sendFunction (PureStructs.UserCommand uid command)  = case PureStructs.text command of 
    "/help" -> sendFunction config 
        (PureStructs.CommonMessage uid (PureStructs.chatID command) (PureStructs.Txt (Config.helpMessage config)) Nothing)
    "/repeat" -> sendFunction config 
        (PureStructs.CommonMessage uid (PureStructs.chatID command) (PureStructs.Buttons PureStructs.buttons) Nothing)
    _ -> pure $ Left LoggerMsgs.cmdMsgFld
processMessage_ config sendFunction (PureStructs.CallbackQuery uid chid txt)  = do 
    let newConfig = setNewRepetition config chid txt 
    sendFunction newConfig 
        (PureStructs.CommonMessage uid chid (PureStructs.Txt (PureStructs.newRepeatText (getNewRepetition txt))) Nothing )
processMessage_ config sendFunction (PureStructs.CommonMessage uid chid cMsg mCap)  = 
    repeatMessage config (PureStructs.CommonMessage uid chid cMsg mCap)(Config.findUserRepeat config chid) sendFunction 

repeatMessage :: (Monad m) => Config.Config -> PureStructs.Message 
    -> Int
    -> (Config.Config -> PureStructs.Message -> m (Either Logger.LogMessage Config.Config))
    -> m (Either Logger.LogMessage Config.Config)
repeatMessage config _ 0 _ = pure $ Right config 
repeatMessage config msg n function = do 
    eiConfig <- function config msg 
    case eiConfig of 
        Left err -> pure $ Left err 
        Right newConfig -> repeatMessage newConfig msg (n-1) function 

setNewRepetition :: Config.Config -> Int -> T.Text -> Config.Config 
setNewRepetition config chid queryText = 
    Config.setUserRepeat config chid (getNewRepetition queryText)

getNewRepetition :: T.Text -> Int 
getNewRepetition "/setRepetition1" = 1 
getNewRepetition "/setRepetition2" = 2 
getNewRepetition "/setRepetition3" = 3 
getNewRepetition "/setRepetition4" = 4 
getNewRepetition "/setRepetition5" = 5 

getResultInfo :: PureStructs.ProcessMessageResult -> T.Text
getResultInfo res = 
    "\n\tUpdate ID: "
    <> (T.pack . show . PureStructs.updID) res 
    <> "\n\tMessage Type: "
    <> PureStructs.msgType res 
    <> maybeChid (PureStructs.mbChatID res)
    <> maybeMsgInfo (PureStructs.mbMessage res)

maybeChid :: Maybe Int -> T.Text
maybeChid Nothing = T.empty
maybeChid (Just chid) = (T.pack . show) chid 

maybeMsgInfo :: Maybe PureStructs.CMessage -> T.Text
maybeMsgInfo Nothing = T.empty
maybeMsgInfo (Just msg) = PureStructs.getMessageType msg

