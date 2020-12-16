{-# LANGUAGE DataKinds #-}
module API.Wrapper where

import Network.HTTP.Req
    ( Url,
      Option,
      Scheme(Https),
      req,
      POST(POST),
      NoReqBody(NoReqBody),
      defaultHttpConfig,
      (/:),
      (=:),
      https,
      lbsResponse,
      responseBody,
      responseStatusCode,
      runReq,
      ReqBodyUrlEnc(ReqBodyUrlEnc) )
import qualified Data.Text as T 
import qualified Data.ByteString.Lazy as BSL  
import Data.Aeson ( eitherDecode ) 
import Data.Maybe ( fromJust )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import qualified Config.Config as Config 
import qualified Logger.Logger as Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs 
import qualified Logic.PureStructs as PureStructs 
import qualified API.VK.Structs as VKStructs 
import qualified API.VK.Cleaners as VKCleaners 
import qualified API.VK.VKData as VKData  
import qualified API.Telegram.TelData as TelData 
import qualified API.Telegram.Cleaners as TelCleaners 


hostPathToUrlScheme :: PureStructs.HostPath -> Url Https 
hostPathToUrlScheme (PureStructs.HostPath hpHost hpPath) = urlScheme (https hpHost) hpPath 

urlScheme :: Url Https -> [T.Text] -> Url Https 
urlScheme acc [] = acc
urlScheme acc (x:xs) = urlScheme (acc /: x) xs 

paramToUrlOption :: PureStructs.Params -> Option Https 
paramToUrlOption (PureStructs.ParamsText key val) = key =: val
paramToUrlOption (PureStructs.ParamsNum key val) = key =: val 
paramToUrlOption (PureStructs.ParamsBool key val) = key =: val 
--paramToUrlOption (PureStructs.ParamsJSON key val) = key =: val 

paramsToUrlOption :: Monad m => [PureStructs.Params] -> m (Option Https) 
paramsToUrlOption params = pure $ mconcat (map paramToUrlOption params)

updateParam :: Config.BotType -> [PureStructs.Params] 
updateParam vk@(Config.VK _ _ _ _ _) = VKData.updateParams vk     
updateParam tel@(Config.Telegram _ _) = TelData.updateParams tel 

data Method = Update | Send deriving Show 

mkHostPath :: Config.Config -> Method -> Maybe PureStructs.PureMessage-> Url Https
mkHostPath config Update _ = 
    case Config.botType config of     
        vk@(Config.VK _ _ _ _ _)-> hostPathToUrlScheme (fromJust $ VKData.updateHostPath vk) 
        tel@(Config.Telegram _ _) -> hostPathToUrlScheme (fromJust $ TelData.updateHostPath tel) 
mkHostPath config Send (Just msg)= 
    case Config.botType config of 
        (Config.VK _ _ _ _ _)-> hostPathToUrlScheme VKData.sendHostPath 
        tel@(Config.Telegram _ _) -> hostPathToUrlScheme $ fromJust (TelData.sendHostPath tel (PureStructs.messageType msg)) 

getPureMessageList :: Config.Config -> Logger.Logger -> IO (Either Logger.LogMessage [PureStructs.PureMessage])
getPureMessageList config logger = getU config >>= byteStringToPureMessageList config logger 

getU :: Config.Config ->  IO (Either Logger.LogMessage BSL.ByteString) 
getU config = do
    params <- paramsToUrlOption (updateParam (Config.botType config))
    runReq defaultHttpConfig $ do     
        response <- req 
            POST
            (mkHostPath config Update Nothing)
            NoReqBody 
            lbsResponse 
            params        
        pure $ pure $ (responseBody response :: BSL.ByteString) 

sendM :: Config.Config -> Logger.Logger -> PureStructs.PureMessage -> IO (Either Logger.LogMessage Config.Config) 
sendM config logger msg = do 
    case PureStructs.mbParams msg of 
        Nothing -> pure $ Right config --Left LoggerMsgs.noParams 
        Just params -> do     
            urlOp <- paramsToUrlOption params 
            basicParams <- VKData.sendBasicParams (Config.botType config)
            basicUrl <- paramsToUrlOption basicParams 
            runReq defaultHttpConfig $ do 
                response <- req 
                    POST 
                    (mkHostPath config Send (Just msg))
                    (ReqBodyUrlEnc mempty)
                    lbsResponse $
                    (basicUrl <> urlOp)
                case responseStatusCode response of 
                    200 -> case Config.botType config of 
                        (Config.VK _ _ _ _ ts) -> do                            
                            let sndMsgResult = eitherDecode (responseBody response) :: Either String VKStructs.VKResult 
                            case sndMsgResult of 
                                Left err -> pure $ Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld (T.pack err))
                                Right (VKStructs.SendMsgError err) -> pure $ 
                                    Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld (VKStructs.errMsg err))
                                Right (VKStructs.SendMsgScs _) -> do 
                                    liftIO $ Logger.botLog logger LoggerMsgs.sndMsgScsVK
                                    pure $ pure (Config.configSetOffset config (PureStructs.updateID msg)) 
                        (Config.Telegram _ _) -> do 
                            liftIO $ Logger.botLog logger LoggerMsgs.sndMsgScsTel
                            pure $ Right (Config.configSetOffset config (succ (PureStructs.updateID msg))) 
                    err -> return $ Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld ((T.pack . show) err))

byteStringToPureMessageList :: Config.Config -> Logger.Logger 
    -> Either Logger.LogMessage BSL.ByteString 
    -> IO (Either Logger.LogMessage [PureStructs.PureMessage])
byteStringToPureMessageList config@(Config.Config (Config.VK _ _ _ _ _)_ _ _ _) logger eiBS = 
    VKCleaners.vkByteStringToPureMessageList config logger eiBS 
byteStringToPureMessageList config@(Config.Config (Config.Telegram _ _)_ _ _ _) logger eiBS = 
    TelCleaners.telByteStringToPureMessageList config logger eiBS 

