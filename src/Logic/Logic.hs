module Logic.Logic where

import qualified Data.Text as T 
import qualified Logic.PureStructs as PureStructs 
import qualified Logger.Logger as Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Config.Config as Config 

type SendFunction a m = 
    (Config.Config -> a -> PureStructs.PureMessage -> m (Either Logger.LogMessage Config.Config))

processMsgs :: (Monad m) => Config.Config -> a    
    -> SendFunction a m 
    -> (Either Logger.LogMessage [PureStructs.PureMessage])
    -> m (Either Logger.LogMessage Config.Config)
processMsgs _ _ _ (Left err)  = pure $ Left err 
processMsgs config logger sendFunction (Right msgs) = do 
    eiConfs <- mapM (processMsgs_ config logger sendFunction) msgs 
    getLastConf config eiConfs  

getLastConf :: Monad m => Config.Config 
    -> [Either Logger.LogMessage Config.Config] 
    -> m (Either Logger.LogMessage Config.Config)
getLastConf config [] = pure $ Right config  
getLastConf _ eiConfs = either (pure . Left) (pure . Right) (last eiConfs)

processMsgs_ :: Monad m => Config.Config -> a
    -> SendFunction a m
    -> PureStructs.PureMessage     
    -> m (Either Logger.LogMessage Config.Config)
processMsgs_ config logger sendFunction msg = case PureStructs.messageType msg of 
    PureStructs.MTEmpty -> pure $ Right (Config.configSetOffset config ((succ . PureStructs.updateID) msg))
    PureStructs.MTUserCommand PureStructs.Help -> sendFunction config logger msg  
    PureStructs.MTUserCommand PureStructs.Repeat -> sendFunction config logger (makeRepeatMsg msg) 
    PureStructs.MTCallbackQuery callbackData -> do 
        let mbChid = PureStructs.mbChatID msg
        maybe processMsgsErr (processMsgsCallback config logger sendFunction msg callbackData) mbChid 
    PureStructs.MTCommon _ -> do 
        let mbChid = PureStructs.mbChatID msg
        maybe processMsgsErr (processMsgsCommon config logger sendFunction msg) mbChid

processMsgsErr :: Monad m => m (Either Logger.LogMessage Config.Config)
processMsgsErr = pure $ Left LoggerMsgs.chidNotFound

processMsgsCallback :: Monad m => Config.Config -> a
    -> SendFunction a m
    -> PureStructs.PureMessage   
    -> T.Text
    -> PureStructs.ChatID   
    -> m (Either Logger.LogMessage Config.Config)
processMsgsCallback config logger function msg callbackData chid = do 
    let newRep = PureStructs.getNewRep callbackData
    let newConfig = Config.setUserRepeat config chid newRep
    function newConfig logger (makeCallbackResponse msg)

processMsgsCommon :: Monad m => Config.Config -> a
    -> SendFunction a m
    -> PureStructs.PureMessage   
    -> PureStructs.ChatID   
    -> m (Either Logger.LogMessage Config.Config)
processMsgsCommon config logger function msg chid = do 
    let newRepeat = Config.findUserRepeat config chid 
    repeatMsg config logger msg newRepeat function

repeatMsg :: Monad m => Config.Config -> a
    -> PureStructs.PureMessage 
    -> Int
    -> SendFunction a m
    -> m (Either Logger.LogMessage Config.Config)
repeatMsg config _ _ 0 _ = pure $ Right config
repeatMsg config logger msg n function =  do
    eiConfig <- function config logger msg 
    either (pure . Left) (repeatMsgScs logger msg n function) eiConfig 

repeatMsgScs :: Monad m => a 
    -> PureStructs.PureMessage  
    -> Int
    -> SendFunction a m
    -> Config.Config
    -> m (Either Logger.LogMessage Config.Config) 
repeatMsgScs logger msg n function config = 
    repeatMsg config logger msg (n-1) function

makeRepeatMsg :: PureStructs.PureMessage -> PureStructs.PureMessage 
makeRepeatMsg msg = PureStructs.PureMessage 
    (PureStructs.MTCommon "Message")
    (PureStructs.updateID msg)
    (PureStructs.mbChatID msg)
    (mconcat [PureStructs.mbParams msg, Just [(PureStructs.ParamsText "text" PureStructs.repeatText)]])

makeCallbackResponse :: PureStructs.PureMessage -> PureStructs.PureMessage  
makeCallbackResponse msg = msg {PureStructs.messageType = PureStructs.MTCommon "Message"} 