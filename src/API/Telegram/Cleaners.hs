module API.Telegram.Cleaners where

import qualified Data.Text as T 
import qualified Data.ByteString.Lazy as BSL 
import Data.Aeson ( decode, eitherDecode )
import qualified API.Telegram.Structs as TStructs 
import qualified Logic.PureStructs as PureStructs 
import qualified Logger.Logger as Logger 
import qualified Config.Config as Config 
import qualified Logger.LoggerMsgs as LoggerMsgs 
import API.Telegram.Cleaners.MakePureMessage
    ( telUpdateToPureMessage' )
import qualified Exceptions.Exceptions as BotEx 
{-
telByteStringToPureMessageList :: Config.Config 
    -> Logger.Logger
    -> Either Logger.LogMessage BSL.ByteString 
    -> IO (Either Logger.LogMessage [PureStructs.PureMessage])
telByteStringToPureMessageList config logger eiBS = 
    decodeByteString logger eiBS >>= telUpdatesToPureMessageList config 

decodeByteString :: Logger.Logger
    -> Either Logger.LogMessage BSL.ByteString     
    -> IO (Either Logger.LogMessage TStructs.TelegramUpdates)
decodeByteString _ (Left err) = pure $ Left err
decodeByteString logger (Right json) = do 
    Logger.botLog logger LoggerMsgs.getTelUpdScs
    let mbTelegramUpdates = decode json :: Maybe TStructs.TelegramUpdates 
    maybe (getUpdErr json) (pure . Right) mbTelegramUpdates

getUpdErr :: BSL.ByteString -> IO (Either Logger.LogMessage TStructs.TelegramUpdates) 
getUpdErr json = do 
    let mbTelegramErr =  eitherDecode json :: Either String TStructs.TelegramUpdatesError
    either parsingErr telError mbTelegramErr

parsingErr :: String -> IO (Either Logger.LogMessage TStructs.TelegramUpdates) 
parsingErr err = pure $ Left (Logger.makeLogMessage LoggerMsgs.getUpdFld (T.pack err))

telError :: TStructs.TelegramUpdatesError -> IO (Either Logger.LogMessage TStructs.TelegramUpdates)  
telError err = pure $ Left 
    (Logger.makeLogMessage LoggerMsgs.getUpdFld 
        ("\n\terror code: " <> (T.pack . show . TStructs.error_code) err 
        <> "\n\terror describtion: " <> TStructs.description err))

telUpdatesToPureMessageList :: Config.Config ->  Either Logger.LogMessage TStructs.TelegramUpdates 
    -> IO (Either Logger.LogMessage [PureStructs.PureMessage])
telUpdatesToPureMessageList _ (Left err) = pure $ Left err 
telUpdatesToPureMessageList config (Right tUpd) = pure $ mapM (telUpdateToPureMessage config) (TStructs.result tUpd)
-}

decodeByteString' :: Logger.Logger
    -> Either BotEx.BotException BSL.ByteString     
    -> IO (Either BotEx.BotException TStructs.TelegramUpdates)
decodeByteString' _ (Left err) = pure $ Left err
decodeByteString' logger (Right json) = do 
    Logger.botLog logger LoggerMsgs.getTelUpdScs
    let mbTelegramUpdates = decode json :: Maybe TStructs.TelegramUpdates 
    maybe (getUpdErr' json) (pure . Right) mbTelegramUpdates

getUpdErr' :: BSL.ByteString -> IO (Either BotEx.BotException TStructs.TelegramUpdates) 
getUpdErr' json = do 
    let mbTelegramErr =  eitherDecode json :: Either String TStructs.TelegramUpdatesError
    either BotEx.throwParseExcept telError' mbTelegramErr

telError' :: TStructs.TelegramUpdatesError -> IO (Either BotEx.BotException TStructs.TelegramUpdates)  
telError' err = pure $ BotEx.throwUpdateExcept  
    (Logger.makeLogMessage LoggerMsgs.getUpdFld 
        ("\n\terror code: " <> (T.pack . show . TStructs.error_code) err 
        <> "\n\terror describtion: " <> TStructs.description err))

telByteStringToPureMessageList' :: Config.Config 
    -> Logger.Logger
    -> Either BotEx.BotException BSL.ByteString 
    -> IO (Either BotEx.BotException [PureStructs.PureMessage])
telByteStringToPureMessageList' config logger eiBS = 
    decodeByteString' logger eiBS >>= telUpdatesToPureMessageList' config 

telUpdatesToPureMessageList' :: Config.Config ->  Either BotEx.BotException TStructs.TelegramUpdates 
    -> IO (Either BotEx.BotException [PureStructs.PureMessage])
telUpdatesToPureMessageList' _ (Left err) = pure $ Left err 
telUpdatesToPureMessageList' config (Right tUpd) = pure $ mapM (telUpdateToPureMessage' config) (TStructs.result tUpd)






