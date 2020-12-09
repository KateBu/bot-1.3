module API.Telegram.Telegram where

import qualified Data.Text as T
import Control.Exception ( bracket )
import qualified Data.ByteString.Lazy as LC 
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpLBS ) 
import Network.HTTP.Client
    ( Request(method, requestBody, requestHeaders),
      Response(responseStatus),
      RequestBody(RequestBodyLBS),
      httpLbs,
      newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings ) 
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson ( decode, eitherDecode, encode )

import qualified Config.Config as Config 
import qualified Logger.Logger as Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Handle.Handle as Handle 
import qualified API.Telegram.Structs as TStructs 
import API.Telegram.Parsers () 
import qualified Logic.PureStructs as PureStructs 
import qualified API.Telegram.Wrapper as TWrapper 
import qualified API.Telegram.Cleaners as Cleaners 


new :: Config.Config -> IO (Handle.Handle IO) 
new config =  pure $ Handle.Handle 
    {
        Handle.hConfig = pure $ Right config 
        , Handle.hLogger = Logger.createLogger (Config.priority config)
        , Handle.hGetUpdates = makeMessages  
        , Handle.hSendMessage_ = sendM_
    }

close :: Handle.Handle m -> IO ()
close _ = pure ()

withHandleNoParams :: Config.Config -> (Handle.Handle IO -> IO a) -> IO a 
withHandleNoParams config = 
    bracket (new config) close 

withHandle :: Config.Config -> (Handle.Handle IO -> c -> IO a) -> c -> IO a 
withHandle config func params = 
    bracket (new config) close (flip func params)

getU :: Config.Config -> IO (Either Logger.LogMessage TStructs.TelegramUpdates)
getU (Config.Config (Config.Telegram tok off) _ _ _ _) = do 
    http <- parseRequest $ "https://api.telegram.org/bot" <> tok <> "/getUpdates?offset=" <> show off <> "&timeout=" <> Config.timeOut
    updRequest <- httpLBS http
    let respBody = getResponseBody updRequest 
    decodeUpd respBody 
    
makeMessages :: Config.Config -> IO (Either Logger.LogMessage [PureStructs.Message])
makeMessages config = do 
    updates <- getU config 
    Cleaners.updatesToPureMessageList updates 

sendM_ :: Config.Config -> PureStructs.Message -> IO (Either Logger.LogMessage Config.Config)
sendM_ config msg = do 
    manager <- newManager tlsManagerSettings
    let respObj =  TWrapper.makeMessageObject msg 
    initialResponse <- parseRequest $ TWrapper.sendMessageHttpRequest config msg 

    let resp = initialResponse {method = "POST"
        , requestBody = RequestBodyLBS $ encode respObj
        , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
        }
    response <- Network.HTTP.Client.httpLbs resp manager 
    case statusCode (responseStatus response) of 
        200 -> pure $ Right (Config.configSetOffset config ((succ . PureStructs.getUid) msg )) 
        err -> return $ Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld ((T.pack . show) err))

decodeUpd :: LC.ByteString -> IO (Either Logger.LogMessage TStructs.TelegramUpdates)
decodeUpd js = case (decode js :: Maybe TStructs.TelegramUpdates) of 
    Just val -> 
            return $ Right val
    Nothing -> case (eitherDecode js :: Either String TStructs.TelegramUpdatesError) of 
        Right val -> return $ Left 
            (Logger.makeLogMessage LoggerMsgs.getUpdFld 
                ("\n\terror code: " <> (T.pack . show . TStructs.error_code) val 
                <> "\n\terror describtion: " <> TStructs.description val))
        Left msg -> return $ Left (Logger.makeLogMessage LoggerMsgs.getUpdFld (T.pack msg))
            
    