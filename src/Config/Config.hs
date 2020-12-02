module Config.Config where

import qualified Data.Text as T 
import qualified Data.Text.IO as TIO 
import qualified Data.Map as Map 
import qualified Data.Configurator as Configurator 
import System.Directory 
import Control.Monad 
import Data.Aeson
import Data.Maybe
import Network.HTTP.Simple 

import qualified Logger.Logger as Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs
import API.VK.Structs
import API.VK.Parsers

type Users = Map.Map Integer Int 

data Config = Config 
    {
        botType :: BotType 
        , helpMessage :: T.Text
        , repetition :: Int 
        , users :: Users 
        , priority :: Logger.Priority
    } deriving Show 

data BotType = Telegram {
        tToken :: String
        , tOffset :: Integer 
    }
    | VK {
        vkToken :: String
        , groupID :: Integer
        , vkKey :: String 
        , vkServer :: String 
        , vkTs :: Integer 
    } deriving Show 

vkApiVersion :: String 
vkApiVersion = "5.90"


parseConfig :: String -> IO (Maybe Config)
parseConfig path = do 
    file <- doesFileExist path
    if not file         
        then pure Nothing 
        else do 
        conf <- Configurator.load [Configurator.Required path]
        botT <- Configurator.lookup conf (T.pack "bot.botType") :: IO (Maybe String)
        rep <- Configurator.lookup conf (T.pack "bot.repetition") :: IO (Maybe Int)
        msg <- Configurator.lookup conf (T.pack "bot.helpMessage") :: IO (Maybe T.Text)
        priority <- Configurator.lookup conf (T.pack "bot.logPriority") :: IO (Maybe String)
        case botT of 
            Just "Telegram" -> do 
                tok <- Configurator.lookup conf (T.pack "bot.telegramToken") :: IO (Maybe String)  
                let tSet = Telegram <$> tok <*> Just 0
                pure $ Config <$> tSet
                    <*>  msg
                    <*> rep 
                    <*> Just Map.empty 
                    <*> (read <$> priority)
            Just "VK" -> do 
                tok <- Configurator.lookup conf (T.pack "bot.VKToken") :: IO (Maybe String)
                group <- Configurator.lookup conf (T.pack "bot.VKGroupID") :: IO (Maybe Integer)
                vkSettings <- getVKSettings group tok
                case vkSettings of
                    Left err -> do 
                        TIO.putStrLn err                        
                        pure Nothing
                    Right (key,serv,ts) -> do 
                        let vkSet = VK <$> tok <*> group <*> Just key <*> Just serv <*> Just ts 
                        pure $  Config <$> vkSet 
                            <*> msg 
                            <*> rep 
                            <*> Just Map.empty
                            <*> (read <$> priority)
            _ -> pure Nothing 

getVKSettings :: Maybe Integer -> Maybe String -> IO (Either T.Text (String, String, Integer))
getVKSettings group tok = do 
    if (isNothing group || isNothing tok) 
        then pure $ Left "VK Config parsing: Coulnd find group id or token"
        else do 
            http <- parseRequest $ "https://api.vk.com/method/groups.getLongPollServer?group_id="
                <> (show . fromJust) group
                <> "&access_token="
                <> fromJust tok 
                <> "&v="   
                <> vkApiVersion
            confSettings <- httpLBS http  
            let respBody = getResponseBody confSettings 
            case (eitherDecode respBody :: Either String VKResponse) of 
                Right val -> case val of 
                    VKResponse k s t -> pure $ Right (k,s,(read t))
                    VKError ec em -> pure $ Left 
                        ("error_code: " 
                                <> (T.pack . show) ec 
                                <> "error_message: "
                                <> em )
                    VKParseError -> pure $ Left "parse Error "
                Left err -> pure $ Left (T.pack err) 





setUserRepeat :: Config -> Integer -> Int -> Config
setUserRepeat config chid newRep = case Map.lookup chid (users config) of 
    Nothing -> addUser chid newRep config
    Just _ -> (addUser chid newRep . deleteUser chid) config 

findUserRepeat :: Config -> Integer -> Int 
findUserRepeat config chid = case Map.lookup chid (users config) of 
    Nothing -> repetition config 
    Just val -> val 


deleteUser :: Integer -> Config -> Config 
deleteUser chid (Config bt hm rep uss prior) = 
    Config bt hm rep (Map.delete chid uss) prior

addUser :: Integer -> Int -> Config -> Config 
addUser chid newRep (Config bt hm rep uss prior) = 
    Config bt hm rep (Map.insert chid newRep uss) prior

configSetOffset :: Config -> Integer -> Config
configSetOffset (Config bt hm rep uss prior) newOffset = 
    case bt of 
        Telegram tok _  -> 
            Config (Telegram tok newOffset) hm rep uss prior
        VK tok group key serv _ -> 
            Config (VK tok group key serv newOffset) hm rep uss prior

