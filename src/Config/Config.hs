module Config.Config where

import qualified Data.Text as T 
import qualified Data.Text.IO as TIO 
import qualified Data.Map as Map 
import qualified Data.Configurator as Configurator 
import System.Directory ( doesFileExist ) 
import Control.Monad () 
import Data.Aeson ( eitherDecode )
import Data.Maybe ( fromJust, isNothing )
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpLBS ) 
import qualified Logger.Logger as Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified API.VK.Structs as VKStructs 


type Users = Map.Map Int Int 
type Token = T.Text 

data Config = Config 
    {
        botType :: BotType 
        , helpMessage :: T.Text
        , repetition :: Int 
        , users :: Users 
        , priority :: Logger.Priority
    } deriving Show 

data BotType = TBot Telegram | VKBot VK 
    deriving Show

data Telegram = Telegram 
    {
        tToken :: Token
        , tOffset :: Int
    } deriving Show  

data VK = VK 
    {
        vkToken :: Token
        , groupID :: Int
        , vkKey :: T.Text 
        , vkServer :: T.Text 
        , vkTs :: Int
    } deriving Show 

vkApiVersion :: T.Text 
vkApiVersion = "5.126"

timeOut :: T.Text 
timeOut = "25"

parseConfig :: String -> IO (Maybe Config)
parseConfig path = do 
    file <- doesFileExist path
    if not file         
        then pure Nothing 
        else do 
        conf <- Configurator.load [Configurator.Required path]
        botT <- Configurator.lookup conf (T.pack "bot.botType") :: IO (Maybe T.Text)
        rep <- Configurator.lookup conf (T.pack "bot.repetition") :: IO (Maybe Int)
        msg <- Configurator.lookup conf (T.pack "bot.helpMessage") :: IO (Maybe T.Text)
        prior <- Configurator.lookup conf (T.pack "bot.logPriority") :: IO (Maybe String)
        tTok <- Configurator.lookup conf (T.pack "bot.telegramToken") :: IO (Maybe Token)  
        vkTok <- Configurator.lookup conf (T.pack "bot.VKToken") :: IO (Maybe Token)
        vkGroup <- Configurator.lookup conf (T.pack "bot.VKGroupID") :: IO (Maybe Int)
        setBotTypeSettings botT vkGroup vkTok tTok >>= initConfig msg rep prior 

setBotTypeSettings :: Maybe T.Text -> Maybe Int -> Maybe Token -> Maybe Token -> IO (Maybe BotType)
setBotTypeSettings (Just "VK") mbGroup mbVKToken _ = do 
    vkSettings <- getVKSettings mbGroup mbVKToken 
    case vkSettings of 
        Left err -> do 
            TIO.putStrLn err 
            pure Nothing 
        Right (key,serv,ts) -> do 
            let vk = (VK <$> mbVKToken 
                    <*> mbGroup 
                    <*> Just key
                    <*> Just serv
                    <*> Just ts)
            pure $ VKBot <$> vk 
setBotTypeSettings (Just "Telegram") _ _ mbTToken =
    pure $ TBot <$> (Telegram <$> mbTToken <*> Just 0)
setBotTypeSettings _ _ _ _ = pure $ Nothing 

getVKSettings :: Maybe Int -> Maybe T.Text -> IO (Either T.Text (T.Text, T.Text, Int))
getVKSettings group tok = do 
    if (isNothing group || isNothing tok) 
        then pure $ Left LoggerMsgs.vkFatalError
        else do 
            http <- parseRequest $ "https://api.vk.com/method/groups.getLongPollServer?group_id="
                <> (show . fromJust) group
                <> "&access_token="
                <> T.unpack (fromJust tok )
                <> "&v="   
                <> T.unpack vkApiVersion
            confSettings <- httpLBS http  
            let respBody = getResponseBody confSettings 
            case (eitherDecode respBody :: Either String VKStructs.VKResponse) of 
                Right val -> case val of 
                    VKStructs.VKResponse k s t -> pure $ Right (k,s,(read t))
                    VKStructs.VKError ec em -> pure $ Left 
                        ("error_code: " 
                                <> (T.pack . show) ec 
                                <> "error_message: "
                                <> em )
                    VKStructs.VKParseError -> pure $ Left "parse Error "
                Left err -> pure $ Left (T.pack err) 

initConfig :: Maybe T.Text -> Maybe Int -> Maybe String -> Maybe BotType 
    -> IO (Maybe Config)
initConfig mbHelpMsg mbRep mbPrior mbBotType = do 
    let config = Config <$> mbBotType 
            <*> mbHelpMsg
            <*> mbRep 
            <*> Just Map.empty 
            <*> (read <$> mbPrior)
    pure config 

getVkGroup :: Config -> Maybe Int
getVkGroup (Config (VKBot bot) _ _ _ _) = Just $ groupID bot
getVkGroup _ = Nothing 

getVkTok :: Config -> Maybe T.Text 
getVkTok (Config (VKBot bot) _ _ _ _) = Just $ vkToken bot  
getVkTok _ = Nothing 

getUid :: Config -> Int
getUid (Config (VKBot bot) _ _ _ _) = vkTs bot  
getUid (Config (TBot bot) _ _ _ _) = tOffset bot 

setUserRepeat :: Config -> Int -> Int -> Config
setUserRepeat config chid newRep = case Map.lookup chid (users config) of 
    Nothing -> addUser chid newRep config
    Just _ -> (addUser chid newRep . deleteUser chid) config 

findUserRepeat :: Config -> Int-> Int 
findUserRepeat config chid = case Map.lookup chid (users config) of 
    Nothing -> repetition config 
    Just val -> val 

deleteUser :: Int -> Config -> Config 
deleteUser chid (Config bt hm rep uss prior) = 
    Config bt hm rep (Map.delete chid uss) prior

addUser :: Int-> Int -> Config -> Config 
addUser chid newRep (Config bt hm rep uss prior) = 
    Config bt hm rep (Map.insert chid newRep uss) prior

configSetOffset :: Config -> Int -> Config
configSetOffset (Config bt hm rep uss prior) newOffset = 
    case bt of 
        TBot (Telegram tok _)  -> 
            Config (TBot $ Telegram tok newOffset) hm rep uss prior
        VKBot (VK tok group key serv _) -> 
            Config (VKBot $ VK tok group key serv newOffset) hm rep uss prior
