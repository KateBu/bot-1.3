module API.VK.VK where


import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception

import qualified Data.ByteString.Lazy as LC 
import qualified Data.Map.Lazy as Map
import Network.HTTP.Simple 
import Network.HTTP.Client
import Network.HTTP.Client.TLS 
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson

import Handle.Handle 
import Config.Config 
import qualified Logger.Logger as Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs
import Logic.PureStructs 
import API.VK.Structs
import API.VK.Parsers


new :: Config -> IO (Handle IO) 
new config =  pure $ Handle 
    {
        hConfig = getConfig config  
        , hLogger = Logger.createLogger (priority config)
        , hGetUpdates = makeMessages config  
        , hSendMessage_ = sendM_
    }

getConfig :: Config -> IO (Either Logger.LogMessage Config)
getConfig config = pure $ Right config 


{-
vkHelpMessage :: T.Text
        , vkRepetition :: Int 
        , vkToken :: String 
        , vkUsers :: Users
        , vkPriority :: Logger.Priority
        , groupID :: Integer
        , vkKey :: T.Text
        , vkServer :: T.Text
        , vkTs :: Int 
-}

getU :: Config -> IO (Either Logger.LogMessage VKUpdates) 
getU config@(VKConfig _ _ tok _ _ group key serve ts) = undefined 

-- {$server}?act=a_check&key={$key}&ts={$ts}&wait=25

getU' :: Config -> IO Config 
getU' config@(VKConfig _ _ tok _ _ group key server ts) = do 
    let http = server <> "?act=a_check&key=" <> key <> "&ts="<> show ts <> "&wait=" <> "25"
    putStrLn http 
    pure  config

makeMessages :: Config -> IO (Either Logger.LogMessage [Message])
makeMessages config = undefined 


sendM_ = undefined 




testConfig :: Config 
testConfig = VKConfig 
        "" 
        1 
        ""
        Map.empty
        Logger.Info
        00
        ""
        ""
        1 














{-getConfig (VKConfig hm rep tok uss prior group Nothing Nothing Nothing) = do 
    http <- parseRequest $ "https://api.vk.com/method/groups.getLongPollServer?group_id=200669666&access_token="
        <> tok 
        <> "&v=5.50"    
    confSettings <- httpLBS http  
    let respBody = getResponseBody confSettings 
    defineVKConfig 
        (VKConfig hm rep tok uss prior group Nothing Nothing Nothing)
        respBody 
-}


{-

defineVKConfig :: Config -> LC.ByteString -> IO (Either Logger.LogMessage Config)
defineVKConfig (VKConfig hm rep tok uss prior group _ _ _) json = case (decode json :: Maybe VKResponse) of 
    Just val -> case val of 
        VKResponse k s t -> pure $ Right (VKConfig hm rep tok uss prior (Just k) (Just s) (Just t))
        VKError ec em -> pure $ Left 
            (Logger.makeLogMessage 
                LoggerMsgs.parseVKConfFld 
                ("error_code: " 
                    <> (T.pack . show) ec 
                    <> "error_message: "
                    <> em ))
        VKParseError -> pure $ Left LoggerMsgs.parseVKConfNoInfo 
    Nothing -> pure $ Left LoggerMsgs.parseVKConfNoInfo 
-}



