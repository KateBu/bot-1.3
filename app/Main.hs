module Main where

import qualified Data.Text as T 
import qualified Data.Text.IO as TIO 

import qualified Config.Config as Config 
import qualified API.Telegram.Telegram as Telegram
import qualified API.VK.VK as VK 
import qualified Handle.Handle as Handle 
import qualified Logger.Logger as Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs 
import qualified Logic.Logic as Logic 
import qualified Logic.PureStructs as PureStructs 


configPath :: String 
configPath = "config.config"

main :: IO ()
main = do
    config <- Config.parseConfig configPath 
    case config of 
        Just existedConfig -> runBot existedConfig
        _ -> TIO.putStrLn LoggerMsgs.fatalConfig 

runBot :: Config.Config -> IO ()
runBot config = do 
    handle <- makeHandle config 
    logger <- Handle.hLogger handle 
    hConf <- Handle.hConfig handle 
    case hConf of 
        Left err -> Logger.botLog logger err 
        Right conf -> do   
            updates <- Handle.hGetUpdates handle conf
            case updates of 
                Left err -> Logger.botLog logger err
                Right upd -> case upd of 
                    [] -> do
                        Logger.botLog logger LoggerMsgs.noUpd
                        nextLoop logger config
                    _ -> do
                        Logger.botLog logger LoggerMsgs.getUpdScs
                        processedMessages <- Logic.processMessages config upd (Handle.hSendMessage_ handle)
                        case processedMessages of 
                            Left err -> do 
                                Logger.botLog logger err
                                nextLoop logger config
                            Right newConfig -> do 
                                Logger.botLog logger LoggerMsgs.sndMsgScs
                                nextLoop logger newConfig         

makeHandle :: Config.Config -> IO (Handle.Handle IO)
makeHandle config = case Config.botType config of 
    Config.Telegram _ _ -> Telegram.new config 
    Config.VK _ _ _ _ _ -> VK.new config 

nextLoop :: Logger.Logger -> Config.Config -> IO ()
nextLoop logger config = do 
    Logger.botLog logger LoggerMsgs.nextLoop
    runBot config 

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
    <> "token: " <> T.pack tok <> "\n"
    <> "offset: " <> T.pack (show off)
showBotSettings (Config.VK tok group key serv ts) = 
    "Bot type: VK \n"
    <> "token: " <> T.pack tok <> "\n"
    <> "group ID: " <> T.pack (show group) <> "\n"
    <> "key: " <> T.pack key <> "\n"
    <> "server: " <> T.pack serv <> "\n"
    <> "ts: " <> T.pack (show ts) <> "\n"

showMessage :: PureStructs.Message -> IO () 
showMessage (PureStructs.CommonMessage uid chid cMsg _) = 
    TIO.putStrLn ( (T.pack $ "\nMessage: \nUpdateID: " <> show uid <> "\nchat id:" <> show chid <> "\n") 
        <> PureStructs.cMsgToText cMsg)
showMessage _ = pure () 
 