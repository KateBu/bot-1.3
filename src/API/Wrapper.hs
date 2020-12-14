{-# LANGUAGE DataKinds #-}
module API.Wrapper where

import Network.HTTP.Req
import qualified Data.Text as T 
--import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BSL  
import Data.Aeson ( eitherDecode ) 
import Data.List.Split ( splitOn )
import System.Random ( Random(random), newStdGen )

import qualified Config.Config as Config 
import qualified Logger.Logger as Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs 
import qualified Logic.PureStructs as PureStructs 
import qualified API.VK.Structs as VKStructs 
import qualified API.VK.Cleaners as VKCleaners 


data HostPath = HostPath T.Text [T.Text] 
    deriving Show 

hostPathToUrlScheme :: HostPath -> Url Https 
hostPathToUrlScheme (HostPath hpHost hpPath) = urlScheme (https hpHost) hpPath 

urlScheme :: Url Https -> [T.Text] -> Url Https 
urlScheme acc [] = acc
urlScheme acc (x:xs) = urlScheme (acc /: x) xs 

paramToUrlOption :: PureStructs.Params -> Option Https 
paramToUrlOption (PureStructs.ParamsText key val) = key =: val 
paramToUrlOption (PureStructs.ParamsNum key val) = key =: val 
paramToUrlOption (PureStructs.ParamsBool key val) = key =: val 
--paramsToUrlOption (ParamsJSON key val) = key =: val 

paramsToUrlOption :: Monad m => [PureStructs.Params] -> m (Option Https) 
paramsToUrlOption params = pure $ mconcat (map paramToUrlOption params)

updateParam :: Config.BotType -> [PureStructs.Params] 
updateParam (Config.VK _ _ key _ ts) = 
    [PureStructs.ParamsText "act" "a_check"
    , PureStructs.ParamsText "key" (T.pack key)
    , PureStructs.ParamsNum "ts" ts
    , PureStructs.ParamsText "wait" (T.pack Config.timeOut)
    ]
updateParam (Config.Telegram _ _) = undefined

hpTelegramUpd :: HostPath
hpTelegramUpd = HostPath "api.telegram.org" ["bot","getUpdates"]

hpVKUpd :: String -> HostPath
hpVKUpd server  = let 
    (host,rest) = span (/='/') (drop 8 server)
    path = filter (/="") (splitOn "/" rest) 
    in HostPath (T.pack host) (map T.pack path) 

hpVKSend :: HostPath
hpVKSend = HostPath "api.vk.com" ["method","messages.send"]

basicParamsSend :: Config.BotType -> IO [PureStructs.Params]
basicParamsSend (Config.VK tok _ _ _ _) = do 
    rid <- getRandonId
    pure $ [PureStructs.ParamsNum "random_id" rid
        , PureStructs.ParamsText "v" (T.pack Config.vkApiVersion)
        , PureStructs.ParamsText "timeout" (T.pack Config.timeOut)
        , PureStructs.ParamsText "access_token" (T.pack tok)
        ]

data Method = Update | Send 
    deriving Show 

mkHostPath :: Config.Config -> Method -> (Url Https)
mkHostPath config Update = 
    case Config.botType config of     
        (Config.VK _ _ _ server _)-> hostPathToUrlScheme (hpVKUpd server) 
        (Config.Telegram _ _) -> hostPathToUrlScheme hpTelegramUpd
mkHostPath config Send = 
    case Config.botType config of 
        (Config.VK _ _ _ _ _)-> hostPathToUrlScheme hpVKSend
        (Config.Telegram _ _) -> undefined 

getPureMessageList :: Config.Config -> IO (Either Logger.LogMessage [PureStructs.PureMessage])
getPureMessageList config = getU config >>= VKCleaners.vkByteStringToPureMessageList

getU :: Config.Config -> IO (Either Logger.LogMessage BSL.ByteString) 
getU config = do
    params <- paramsToUrlOption (updateParam (Config.botType config))
    runReq defaultHttpConfig $ do     
        response <- req 
            POST
            (mkHostPath config Update)
            NoReqBody 
            lbsResponse 
            params
        pure $ pure $ (responseBody response :: BSL.ByteString) 

sendM :: Config.Config -> [PureStructs.Params] -> IO (Either Logger.LogMessage ()) 
sendM config params = do 
    urlOp <- paramsToUrlOption params 
    basicParams <- basicParamsSend (Config.botType config)
    basicUrl <- paramsToUrlOption basicParams 
    runReq defaultHttpConfig $ do 
        response <- req 
            POST 
            (mkHostPath config Send)
            NoReqBody 
            lbsResponse $
            (basicUrl <> urlOp)
        case responseStatusCode response of 
            200 -> case Config.botType config of 
                (Config.VK _ _ _ _ _) -> do
                    let sndMsgResult = eitherDecode (responseBody response) :: Either String VKStructs.VKResult 
                    case sndMsgResult of 
                        Left err -> pure $ Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld (T.pack err))
                        Right (VKStructs.SendMsgError err) -> pure $ 
                            Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld (VKStructs.errMsg err))
                        Right (VKStructs.SendMsgScs _) -> pure $ pure ()                           
            err -> return $ Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld ((T.pack . show) err))

getRandonId :: IO Int
getRandonId = do 
    gen <- newStdGen 
    pure $ ( (fst . random) gen :: Int)


-- the functions below will be removed soon 

getUpdate :: Config.Config -> IO (Either Logger.LogMessage [PureStructs.Message]) 
getUpdate config = do 
    respBody <- getU config 
    case respBody of 
        Left err -> pure $ Left err 
        Right val -> do 
            decoded <- decodeUpd (Config.botType config) val
            case decoded of 
                Left err -> pure $ Left err 
                Right res -> case Config.botType config of 
                    (Config.VK _ _ _ _ ts) -> VKCleaners.updatesToPureMessageList $ (res, ts) 

decodeUpd :: Config.BotType -> BSL.ByteString -> IO (Either Logger.LogMessage VKStructs.VKUpdates)
decodeUpd (Config.VK _ _ _ _ _) json = case (eitherDecode json :: Either String VKStructs.VKUpdates) of 
    Right (VKStructs.VKUpdateError err_code _) -> case err_code of 
        1 -> pure $ Left LoggerMsgs.vkUpdatesFailed1  
        2 -> pure $ Left LoggerMsgs.vkUpdatesFailed2
        3 -> pure $ Left LoggerMsgs.vkUpdatesFailed3
        _ -> pure $ Left LoggerMsgs.vkUpdatesFailed4 
    Right upd -> pure $ Right upd 
    Left err -> pure $ Left (Logger.LogMessage Logger.Error ("decode vk update failed: " <> (T.pack err)))