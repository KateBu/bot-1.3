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
import API.VK.Cleaners
import API.VK.Wrapper




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


getU :: Config -> IO (Either Logger.LogMessage VKUpdates) 
getU (Config (VK _ _ key server ts) _ _ _ _) = do 
    http <- parseRequest $ server 
        <> "?act=a_check&key=" 
        <> key 
        <> "&ts="
        <> show ts 
        <> "&wait=" 
        <> timeOut        
    updRequest <- httpLBS http 
    let respBody = getResponseBody updRequest 
    decodeUpd respBody 



decodeUpd :: LC.ByteString -> IO (Either Logger.LogMessage VKUpdates)
decodeUpd json = case (eitherDecode json :: Either String VKUpdates) of 
    Right (VKUpdateError err_code _) -> case err_code of 
        1 -> pure $ Left LoggerMsgs.vkUpdatesFailed1 
        2 -> pure $ Left LoggerMsgs.vkUpdatesFailed2
        3 -> pure $ Left LoggerMsgs.vkUpdatesFailed3
    Right upd -> pure $ Right upd 
    Left err -> pure $ Left (Logger.LogMessage Logger.Error ("decode vk update failed: " <> (T.pack err)))


makeMessages :: Config -> IO (Either Logger.LogMessage [Message])
makeMessages config@(Config (VK _ _ _ _ ts) _ _ _ _) = do 
    vkUpd <- getU config 
    case vkUpd of 
        Left err -> pure $ Left err 
        Right val -> updatesToPureMessageList $ (val, ts) 


sendM_ :: Config -> Message -> IO (Either Logger.LogMessage Config)
sendM_ config message = do
    manager  <- newManager tlsManagerSettings
    let respObj = makeMessageObject message 
    initialResponse <- parseRequest $ sendMessageHttpRequest config 

    let resp = initialResponse {method = "POST"
        , requestBody = RequestBodyLBS $ encode respObj
        , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
        }
    response <- Network.HTTP.Client.httpLbs resp manager 

    case statusCode (responseStatus response) of 
        200 -> pure $ Right (configSetOffset config ((succ . getUid) message )) 
        err -> return $ Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld ((T.pack . show) err))


timeOut :: String 
timeOut = "25"









