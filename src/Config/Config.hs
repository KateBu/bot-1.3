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
import qualified API.VK.Structs as VKStructs 
import API.VK.Parsers ()

type Users = Map.Map Int Int 

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
        , tOffset :: Int
    }
    | VK {
        vkToken :: String
        , groupID :: Int
        , vkKey :: String 
        , vkServer :: String 
        , vkTs :: Int
    } deriving Show 

vkApiVersion :: String 
vkApiVersion = "5.126"

timeOut :: String 
timeOut = "25"

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
                group <- Configurator.lookup conf (T.pack "bot.VKGroupID") :: IO (Maybe Int)
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

getVKSettings :: Maybe Int -> Maybe String -> IO (Either T.Text (String, String, Int))
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

getVkGroup :: Config -> Maybe Int
getVkGroup (Config (VK _ group _ _ _) _ _ _ _) = Just group 
getVkGroup _ = Nothing 

getVkTok :: Config -> Maybe String 
getVkTok (Config (VK tok _ _ _ _) _ _ _ _) = Just tok 
getVkTok _ = Nothing 

getVkTs :: Config -> Maybe Int
getVkTs (Config (VK _ _ _ _ ts) _ _ _ _) = Just ts 
getVkTs _ = Nothing

setVkSettings :: Config ->  Either T.Text (String, String, Int) -> Either Logger.LogMessage Config 
setVkSettings _ (Left txt) = Left (Logger.LogMessage Logger.Error txt)
setVkSettings (Config (VK tok group _ _ _) hm rep uss prior) (Right (key, serv, ts)) = Right $ 
    Config (VK tok group key serv ts ) hm rep uss prior 

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
        Telegram tok _  -> 
            Config (Telegram tok newOffset) hm rep uss prior
        VK tok group key serv _ -> 
            Config (VK tok group key serv newOffset) hm rep uss prior

