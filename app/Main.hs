module Main where

import qualified Data.Text as T 
import qualified Data.Text.IO as TIO 
import Control.Concurrent

import Config.Config 
import qualified API.Telegram.Telegram as Telegram
import Handle.Handle
import Logger.Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs 
import API.Telegram.Cleaners



configPath :: String 
configPath = "local.config"

main :: IO ()
main = do
    config <- parseConfig configPath 
    case config of 
        Just config -> runBot config 
        _ -> putStrLn $ "FATAL ERROR: couldn't find or read the config file," 
            <> " check the path to the file and the information inside it"


runBot :: Config -> IO ()
runBot config@(TConfig _ _ _ _ _ _) = do 
    logger <- Telegram.withHandleNoParams config hLogger
    updates <- Telegram.withHandleNoParams config hGetUpdates 
    case updates of 
        Left err -> botLog logger (LogMessage Error (LoggerMsgs.getLogMsg "getUpdFld" <> err))
        Right upd -> do
            botLog logger (LogMessage Debug (LoggerMsgs.getLogMsg "getUpdScs"))
            --maybeMessageList <- updatesToPureMessageList logger upd 
            --pure ()
runBot _ = putStrLn "Only Telegram bot API defined"






showConfig :: Config -> IO ()
showConfig (TConfig help off rep tok us prior) = do 
    TIO.putStrLn $ "Config file parsed: " 
        <> help <> "\n" <> T.pack (show off) <> "\n" <> T.pack (show rep) <> "\n"
        <> T.pack tok <> "\n" <> T.pack (show us) <> "\n" <> T.pack (show prior) 