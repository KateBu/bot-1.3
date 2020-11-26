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
import Logic.Logic 



configPath :: String 
configPath = "local.config"

main :: IO ()
main = do
    config <- parseConfig configPath 
    case config of 
        Just config -> runBot config 
        _ -> TIO.putStrLn LoggerMsgs.fatalConfig 


runBot :: Config -> IO ()
runBot config = do 
    handle <- makeHandle config 
    logger <- hLogger handle 
    updates <- hGetUpdates handle 
    case updates of 
        Left err -> botLog logger err
        Right upd -> case upd of 
            [] -> do
                botLog logger LoggerMsgs.noUpd
                nextLoop logger config
            _ -> do
                botLog logger LoggerMsgs.getUpdScs
                processedMessages <- processMessages config upd (hSendMessage_ handle)
                case processedMessages of 
                    Left err -> do 
                        botLog logger err
                        nextLoop logger config
                    Right newConfig -> do 
                        botLog logger LoggerMsgs.sndMsgScs
                        nextLoop logger newConfig

                





makeHandle :: Config -> IO (Handle IO)
makeHandle config@(TConfig _ _ _ _ _ _) = Telegram.new config 
makeHandle config = undefined


nextLoop :: Logger -> Config -> IO ()
nextLoop logger config = do 
    botLog logger LoggerMsgs.nextLoop
    threadDelay 3000000 
    runBot config 




showConfig :: Config -> IO ()
showConfig (TConfig help off rep tok us prior) = do 
    TIO.putStrLn $ "Config file parsed: " 
        <> help <> "\n" <> T.pack (show off) <> "\n" <> T.pack (show rep) <> "\n"
        <> T.pack tok <> "\n" <> T.pack (show us) <> "\n" <> T.pack (show prior) 