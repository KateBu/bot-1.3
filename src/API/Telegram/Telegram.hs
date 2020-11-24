module API.Telegram.Telegram where

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


import Config.Config 
import qualified Logger.Logger as Logger 
import Handle.Handle 
import API.Telegram.Structs 
import API.Telegram.Parsers
import Logic.PureStructs

import qualified API.Telegram.Cleaners as Cleaners 


new :: Config -> IO (Handle IO) 
new config =  pure $ Handle 
    {
        hConfig = pure config 
        , hLogger = Logger.createLogger (priority config)
        , hGetUpdates = makeMessages config  
        , hSendMessage = sendM config
        , hSendMessage_ = sendM_ config
        , hSetOffset = setOffset config 
        , hEditUsers =undefined 
    }

close :: Handle m -> IO ()
close _ = pure ()

withHandleNoParams :: Config -> (Handle IO -> IO a) -> IO a 
withHandleNoParams config = 
    bracket (new config) close 

withHandle :: Config -> (Handle IO -> c -> IO a) -> c -> IO a 
withHandle config func params = 
    bracket (new config) close (flip func params)


getU :: Config -> IO (Either Err TelegramUpdates)
getU (TConfig _ off _ tok _ _) = do 
    http <- parseRequest $ "https://api.telegram.org/bot" <> tok <> "/getUpdates?offset=" <> show off
    updRequest <- httpLBS http
    let respBody = getResponseBody updRequest 
    decodeUpd respBody 
    
makeMessages :: Config -> IO (Either Err [Message])
makeMessages config = do 
    updates <- getU config 
    Cleaners.updatesToPureMessageList updates 

    

sendM :: Config -> Message -> IO (Maybe Config)
sendM = undefined

sendM_ :: Config -> Message -> IO (Maybe ())
sendM_ = undefined

setOffset :: (Monad m) => Config -> Integer -> m Config 
setOffset (TConfig msg _ rep tok us prior) newOffset = 
    pure $ (TConfig msg newOffset rep tok us prior)


decodeUpd :: LC.ByteString -> IO (Either Err TelegramUpdates)
decodeUpd js = case (decode js :: Maybe TelegramUpdates) of 
    Just val -> 
            return $ Right val
    Nothing -> case (eitherDecode js :: Either String TelegramUpdatesError) of 
        Right val -> return $ Left ("\n\terror code: " <> (T.pack . show . error_code) val 
            <> "\n\terror describtion: " <> description val)
        Left msg -> return $ Left (T.pack msg)
            
    


makeErrorMsg :: TelegramUpdatesError -> T.Text
makeErrorMsg (TelegramUpdatesError code desc) = "error code: " 
    <> T.pack (show code)
    <> ", error describtion: "
    <> desc 