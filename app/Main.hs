module Main where

import qualified Data.Text as T 
import qualified Data.Text.IO as TIO 
import Control.Concurrent

import Config.Config 
import qualified API.Telegram.Telegram as Telegram
import Handle.Handle
import Logger.Logger 
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
    updates <- Telegram.withHandle config hGetUpdates logger 
    case updates of 
        Nothing -> botLog logger (LogMessage Info "exit program, couldn't get updates")
        Just upd -> do
            maybeMessageList <- updatesToPureMessageList logger upd 
            pure ()
runBot _ = putStrLn "Only Telegram bot API defined"






showConfig :: Config -> IO ()
showConfig (TConfig help off rep tok us prior) = do 
    TIO.putStrLn $ "Config file parsed: " 
        <> help <> "\n" <> T.pack (show off) <> "\n" <> T.pack (show rep) <> "\n"
        <> T.pack tok <> "\n" <> T.pack (show us) <> "\n" <> T.pack (show prior) 