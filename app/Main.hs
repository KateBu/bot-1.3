module Main where

import qualified Data.Text.IO as TIO 
import qualified API.Bot as Bot 
import qualified Config.Config as Config 
import qualified Logger.LoggerMsgs as LoggerMsgs 


configPath :: String 
configPath = "local.config"

main :: IO ()
main = do
    config <- Config.parseConfig configPath 
    case config of 
        Just existedConfig -> Bot.runBot existedConfig
        _ -> TIO.putStrLn LoggerMsgs.fatalConfig 






-- the code below will be removed soon 


{-
showTS :: Config.Config -> IO ()
showTS (Config.Config (Config.VK _ _ _ _ ts) _ _ _ _) = 
    TIO.putStrLn ("\n------------\nNew ts: " <> (T.pack . show) ts <> "\n-------------\n")
showTS _ = putStrLn "\nIt's not a VK bot\n"

showConfig :: Config.Config -> IO ()
showConfig (Config.Config bt hm rep uss prior) = do 
    TIO.putStrLn $ "Config file parsed: " 
        <> showBotSettings bt <> "\n"
        <> "help message: " <> hm <> "\n"
        <> "repetition: " <> T.pack (show rep) <> "\n"
        <> "users: " <> T.pack (show uss) <> "\n"
        <> "priority: " <> T.pack (show prior)

showBotSettings :: Config.BotType -> T.Text
showBotSettings (Config.Telegram tok off) = 
    "Bot type: Telegram \n" 
    <> "token: " <> tok <> "\n"
    <> "offset: " <> T.pack (show off)
showBotSettings (Config.VK tok group key serv ts) = 
    "Bot type: VK \n"
    <> "token: " <>tok <> "\n"
    <> "group ID: " <> T.pack (show group) <> "\n"
    <> "key: " <> key <> "\n"
    <> "server: " <>serv <> "\n"
    <> "ts: " <> T.pack (show ts) <> "\n"
 -}