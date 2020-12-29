module Config.MakeConfigFunctions where

import qualified Data.Text as T 
import qualified Data.Text.IO as TIO 
import qualified Data.Map as Map 
import qualified Data.Configurator as Configurator 
import System.Directory ( doesFileExist ) 
import Control.Monad () 
import Data.Aeson ( eitherDecode )
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpLBS ) 
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified API.VK.Structs as VKStructs 
import Config.ConfigStructs as Config
    ( Token, VK(VK), Telegram(Telegram), BotType(..), Config(Config) )
import Config.ConfigData as Config ( vkApiVersion, vkLongPollUrl )

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
    either vkSettingsError (vkSettingsScs mbGroup mbVKToken) vkSettings
setBotTypeSettings (Just "Telegram") _ _ mbTToken =
    pure $ TBot <$> (Telegram <$> mbTToken <*> Just 0)
setBotTypeSettings _ _ _ _ = pure $ Nothing 

vkSettingsError :: T.Text -> IO (Maybe BotType) 
vkSettingsError err = do 
    TIO.putStrLn err 
    pure Nothing 

vkSettingsScs ::   Maybe Int -> Maybe Token -> (T.Text, T.Text, Int) -> IO (Maybe BotType)  
vkSettingsScs mbGroup mbVKToken (key,serv,ts) = do 
    let vk = (VK <$> mbVKToken 
            <*> mbGroup 
            <*> Just key
            <*> Just serv
            <*> Just ts)
    pure $ VKBot <$> vk 

getVKSettings :: Maybe Int -> Maybe T.Text -> IO (Either T.Text (T.Text, T.Text, Int))
getVKSettings (Just group) (Just tok) = do     
    http <- parseRequest $ makeVkLonpPollUrl group tok 
    confSettings <- httpLBS http  
    let respBody = getResponseBody confSettings 
    let eiResponse  = eitherDecode respBody :: Either String VKStructs.VKResponse
    either makeVKConfigError tryMakeVKSettings eiResponse 
getVKSettings _ _ = pure $ Left LoggerMsgs.vkFatalError

makeVkLonpPollUrl :: Int -> T.Text -> String 
makeVkLonpPollUrl group tok = vkLongPollUrl  
    <> show group
    <> "&access_token="
    <> T.unpack tok 
    <> "&v="    
    <> T.unpack vkApiVersion

makeVKConfigError :: String -> IO (Either T.Text (T.Text, T.Text, Int))
makeVKConfigError err = pure $ Left (T.pack err)

tryMakeVKSettings :: VKStructs.VKResponse -> IO (Either T.Text (T.Text, T.Text, Int)) 
tryMakeVKSettings (VKStructs.VKResponse (VKStructs.LongPollResponse k s t)) = pure $ Right (k,s,(read t))
tryMakeVKSettings (VKStructs.VKError (VKStructs.ResponseError ec em)) = pure $ Left 
    ("error_code: " 
        <> (T.pack . show) ec 
        <> "error_message: "
        <> em )
tryMakeVKSettings VKStructs.VKParseError = pure $ Left "VK Parse error"


initConfig :: Maybe T.Text -> Maybe Int -> Maybe String -> Maybe BotType 
    -> IO (Maybe Config)
initConfig mbHelpMsg mbRep mbPrior mbBotType = do 
    let config = Config <$> mbBotType 
            <*> mbHelpMsg
            <*> checkRepNumber mbRep 
            <*> Just Map.empty 
            <*> (read <$> mbPrior)
    pure config 

checkRepNumber :: Maybe Int -> Maybe Int 
checkRepNumber Nothing = Nothing 
checkRepNumber (Just val) 
    | val <= 1      = Just 1 
    | val >= 5      = Just 5 
    | otherwise     = Just val 