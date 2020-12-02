module Main where

import qualified Data.Text as T 
import qualified Data.Text.IO as TIO 
import Control.Concurrent

import Config.Config 
import qualified API.Telegram.Telegram as Telegram
import qualified API.VK.VK as VK 
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
    --hConf <- hConfig handle 
    showConfig config 
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
makeHandle config = case botType config of 
    Telegram _ _ -> Telegram.new config 
    VK _ _ _ _ _ -> VK.new config 


nextLoop :: Logger -> Config -> IO ()
nextLoop logger config = do 
    botLog logger LoggerMsgs.nextLoop
    threadDelay 3000000 
    runBot config 




showConfig :: Config -> IO ()
showConfig (Config bt hm rep uss prior) = do 
    TIO.putStrLn $ "Config file parsed: " 
        <> showBotSettings bt <> "\n"
        <> "help message: " <> hm <> "\n"
        <> "repetition: " <> T.pack (show rep) <> "\n"
        <> "users: " <> T.pack (show uss) <> "\n"
        <> "priority: " <> T.pack (show prior)


showBotSettings :: BotType -> T.Text
showBotSettings (Telegram tok off) = 
    "Bot type: Telegram \n" 
    <> "token: " <> T.pack tok <> "\n"
    <> "offset: " <> T.pack (show off)
showBotSettings (VK tok group key serv ts) = 
    "Bot type: VK \n"
    <> "token: " <> T.pack tok <> "\n"
    <> "group ID: " <> T.pack (show group) <> "\n"
    <> "key: " <> T.pack key <> "\n"
    <> "server: " <> T.pack serv <> "\n"
    <> "ts: " <> T.pack (show ts) <> "\n"
