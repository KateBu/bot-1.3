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
            Right conf -> pure $ Right conf 

processMessage_ :: (Monad m) => Config.Config 
    -> (Config.Config -> PureStructs.Message -> m (Either Logger.LogMessage Config.Config)) 
    -> PureStructs.Message     
    -> m (Either Logger.LogMessage Config.Config)
processMessage_ config _ (PureStructs.EmptyMessage uid)  = 
    pure $ Right (Config.configSetOffset config (succ uid))
processMessage_ config sendFunction (PureStructs.UserCommand uid command)  = case PureStructs.text command of 
    "/help" -> sendFunction config 
        (PureStructs.CommonMessage uid (PureStructs.chatID command) (makeHelpMessage config) Nothing)
    "/repeat" -> sendFunction config 
        (PureStructs.CommonMessage uid (PureStructs.chatID command) makeRepeatMessage Nothing)
    _ -> pure $ Left LoggerMsgs.cmdMsgFld
processMessage_ config sendFunction (PureStructs.CallbackQuery uid chid txt)  = do 
    let newConfig = setNewRepetition config chid txt 
    sendFunction newConfig 
        (PureStructs.CommonMessage uid chid (makeCallbackResponse txt) Nothing )
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
getNewRepetition txt 
    | txt == PureStructs.rep1 = 1 
    | txt == PureStructs.rep2 = 2 
    | txt == PureStructs.rep3 = 2 
    | txt == PureStructs.rep4 = 2 
    | otherwise = 5 

makeHelpMessage :: Config.Config -> PureStructs.ComMessage
makeHelpMessage config = PureStructs.defaultComMsg {
        PureStructs.commonMsgType = "Message"
        , PureStructs.mbText = Just (Config.helpMessage config)
    }

makeRepeatMessage :: PureStructs.ComMessage
makeRepeatMessage = PureStructs.defaultComMsg {
        PureStructs.commonMsgType = "Message"
        , PureStructs.mbText = Just PureStructs.repeatText 
        , PureStructs.buttons = True 
    }

makeCallbackResponse :: T.Text -> PureStructs.ComMessage 
makeCallbackResponse txt = PureStructs.defaultComMsg {
        PureStructs.commonMsgType = "Message"
        , PureStructs.mbText = Just $ PureStructs.newRepeatText (getNewRepetition txt)
    }

maybeChid :: Maybe Int -> T.Text
maybeChid Nothing = T.empty
maybeChid (Just chid) = (T.pack . show) chid 

maybeMsgInfo :: Maybe PureStructs.ComMessage -> T.Text
maybeMsgInfo Nothing = T.empty
maybeMsgInfo (Just msg) = PureStructs.commonMsgType msg 
