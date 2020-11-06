module Config.Config where

import qualified Data.Text as T 
import qualified Data.Map as Map 
import qualified Data.Configurator as Configurator 
import System.Directory 
import Control.Monad 

import qualified Logger.Logger as Logger 

type Users = Map.Map Integer Int 

data Config = TConfig {
        helpMessage :: T.Text 
        , offset :: Integer
        , repetition :: Int 
        , token :: String 
        , users :: Users 
        , priority :: Logger.Priority 
        }

    | VKConfig
    deriving Show 


parseConfig :: String -> IO (Maybe Config)
parseConfig path = do 
    file <- doesFileExist path
    if not file         
        then pure Nothing 
        else do 
        conf <- Configurator.load [Configurator.Required path]
        botT <- Configurator.lookup conf (T.pack "bot.botType") :: IO (Maybe String)
        case botT of 
            Just "Telegram" -> do 
                rep <- Configurator.lookup conf (T.pack "bot.repetition") :: IO (Maybe Int)
                tok <- Configurator.lookup conf (T.pack "bot.telegramToken") :: IO (Maybe String)
                msg <- Configurator.lookup conf (T.pack "bot.helpMessage") :: IO (Maybe T.Text)
                priority <- Configurator.lookup conf (T.pack "bot.logPriority") :: IO (Maybe String)
                pure $ TConfig <$> msg 
                    <*> Just 0 
                    <*> rep
                    <*> tok 
                    <*> Just Map.empty 
                    <*> (read <$> priority)                    
            _ -> pure Nothing 