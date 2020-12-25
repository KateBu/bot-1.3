module Logic.Logic where

import qualified Logic.PureStructs as PureStructs 
import qualified Logger.Logger as Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Config.Config as Config 

processMsgs :: (Monad m) => Config.Config -> a    
    -> (Config.Config -> a -> PureStructs.PureMessage -> m (Either Logger.LogMessage Config.Config))
    -> (Either Logger.LogMessage [PureStructs.PureMessage])
    -> m (Either Logger.LogMessage Config.Config)
processMsgs _ _ _ (Left err)  = pure $ Left err 
processMsgs config logger sendFunction (Right msgs) = do 
    eiConfs <- mapM (processMsgs_ config logger sendFunction) msgs 
    case eiConfs of 
        [] -> pure $ Right config 
        _ -> case last eiConfs of 
            Left err -> pure $ Left err
            Right conf -> pure $ Right conf   

processMsgs_ :: Monad m => Config.Config -> a
    -> (Config.Config -> a -> PureStructs.PureMessage -> m (Either Logger.LogMessage Config.Config)) 
    -> PureStructs.PureMessage     
    -> m (Either Logger.LogMessage Config.Config)
processMsgs_ config logger sendFunction msg = case PureStructs.messageType msg of 
    PureStructs.MTEmpty -> pure $ Right (Config.configSetOffset config ((succ . PureStructs.updateID) msg))
    PureStructs.MTUserCommand PureStructs.Help -> sendFunction config logger msg  
    PureStructs.MTUserCommand PureStructs.Repeat -> sendFunction config logger (makeRepeatMsg msg) 
    PureStructs.MTCallbackQuery callbackData -> do 
        let mbChid = PureStructs.mbChatID msg
        case mbChid of 
            Nothing -> pure $ Left LoggerMsgs.chidNotFound
            Just chid -> do 
                let newRep = PureStructs.getNewRep callbackData
                let newConfig = Config.setUserRepeat config chid newRep
                sendFunction newConfig logger (makeCallbackResponse msg)
    PureStructs.MTCommon _ -> do 
        case PureStructs.mbChatID msg of 
            Nothing -> pure $ Left LoggerMsgs.noChatId
            Just chid -> repeatMsg config logger msg (Config.findUserRepeat config chid) sendFunction

repeatMsg :: Monad m => Config.Config -> a
    -> PureStructs.PureMessage 
    -> Int
    -> (Config.Config -> a -> PureStructs.PureMessage -> m (Either Logger.LogMessage Config.Config))
    -> m (Either Logger.LogMessage Config.Config)
repeatMsg config _ _ 0 _ = pure $ Right config
repeatMsg config logger msg n function =  do
    eiConfig <- function config logger msg 
    case eiConfig of 
        Left err -> pure $ Left err  
        Right newConfig -> repeatMsg newConfig logger msg (n-1) function

makeRepeatMsg :: PureStructs.PureMessage -> PureStructs.PureMessage 
makeRepeatMsg msg = PureStructs.PureMessage 
    (PureStructs.MTCommon "Message")
    (PureStructs.updateID msg)
    (PureStructs.mbChatID msg)
    (mconcat [PureStructs.mbParams msg, Just [(PureStructs.ParamsText "text" PureStructs.repeatText)]])

makeCallbackResponse :: PureStructs.PureMessage -> PureStructs.PureMessage  
makeCallbackResponse msg = msg {PureStructs.messageType = PureStructs.MTCommon "Message"} 