module API.VK.VK where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LC 
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpLBS ) 
import Network.HTTP.Client
    ( Request(method, requestHeaders),
      Response(responseStatus),
      httpLbs,
      newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings ) 
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson ( eitherDecode )
import System.Random ( Random(random), newStdGen ) 
import Data.Maybe ( fromJust ) 

import qualified Handle.Handle as Handle 
import qualified Config.Config as Config 
import qualified Logger.Logger as Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs 
import qualified API.VK.Structs as VKStructs 
import qualified API.VK.Cleaners as VKCleaners 
import qualified API.VK.Wrapper as VKWrappers 


new :: Config.Config -> IO (Handle.Handle IO) 
new config =  pure $ Handle.Handle 
    {
        Handle.hConfig = getConfig config  
        , Handle.hLogger = Logger.createLogger (Config.priority config)
        , Handle.hGetUpdates = makeMessages  
        , Handle.hSendMessage_ = sendM_
    }

getConfig :: Config.Config -> IO (Either Logger.LogMessage Config.Config)
getConfig config = pure $ Right config 

getU :: Config.Config -> IO (Either Logger.LogMessage VKStructs.VKUpdates) 
getU config@(Config.Config (Config.VK _ _ key server ts) _ _ _ _) = do 
    http <- parseRequest $ server 
        <> "?act=a_check&key=" 
        <> key 
        <> "&ts="
        <> show ts 
        <> "&wait=" 
        <> Config.timeOut        
    updRequest <- httpLBS http 
    let respBody = getResponseBody updRequest 
    decoded <- decodeUpd respBody
    case decoded of 
        Right (VKStructs.VKUpdateError _ mbNewTs) -> 
            getU (Config.configSetOffset config (fromJust mbNewTs))
        Right val -> do 
            pure $ Right val 
        Left err -> pure $ Left err 
getU _ = pure $ Left LoggerMsgs.getUpdFld

decodeUpd :: LC.ByteString -> IO (Either Logger.LogMessage VKStructs.VKUpdates)
decodeUpd json = case (eitherDecode json :: Either String VKStructs.VKUpdates) of 
    Right (VKStructs.VKUpdateError err_code _) -> case err_code of 
        1 -> pure $ Left LoggerMsgs.vkUpdatesFailed1  
        2 -> pure $ Left LoggerMsgs.vkUpdatesFailed2
        3 -> pure $ Left LoggerMsgs.vkUpdatesFailed3
        _ -> pure $ Left LoggerMsgs.vkUpdatesFailed4 
    Right upd -> pure $ Right upd 
    Left err -> pure $ Left (Logger.LogMessage Logger.Error ("decode vk update failed: " <> (T.pack err)))

makeMessages :: Config.Config -> IO (Either Logger.LogMessage [PureStructs.Message])
makeMessages config@(Config.Config (Config.VK _ _ _ _ ts) _ _ _ _) = do 
    vkUpd <- getU config 
    case vkUpd of 
        Left err -> pure $ Left err 
        Right val -> VKCleaners.updatesToPureMessageList $ (val, ts) 
makeMessages _ = pure $ Left LoggerMsgs.unreadableConfig

sendM_ :: Config.Config -> PureStructs.Message -> IO (Either Logger.LogMessage Config.Config)
sendM_ config message = do
    random_id <- getRandonId 
    manager  <- newManager tlsManagerSettings
    initialResponse <- parseRequest $ VKWrappers.sendMessageHttpRequest config 
        <> VKWrappers.makeRequestBody message random_id     
    let resp = initialResponse { method = "POST"
        , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
        }
    response <- Network.HTTP.Client.httpLbs resp manager   
    case statusCode (responseStatus response) of 
        200 -> do 
            print (getResponseBody response)
            let sndMsgResult = eitherDecode (getResponseBody response) :: Either String VKStructs.VKResult 
            case sndMsgResult of 
                Left err -> pure $ Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld (T.pack err))
                Right (VKStructs.SendMsgError err) -> pure $ 
                    Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld (VKStructs.errMsg err))
                Right (VKStructs.SendMsgScs _) -> pure $ 
                    Right (Config.configSetOffset config ((succ . PureStructs.getMsgUid) message ))
        err -> return $ Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld ((T.pack . show) err))

getRandonId :: IO Int
getRandonId = do 
    gen <- newStdGen 
    pure $ ( (fst . random) gen :: Int)
