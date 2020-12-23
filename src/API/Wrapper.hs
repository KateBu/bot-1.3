-- {-# LANGUAGE DataKinds #-}
module API.Wrapper where

import Network.HTTP.Req
    ( LbsResponse,
      Url,
      FormUrlEncodedParam,
      ReqBodyMultipart,
      ReqBodyUrlEnc(..),
      (/:),
      (=:),
      defaultHttpConfig,
      https,
      lbsResponse,
      req,
      reqBodyMultipart,
      responseBody,
      responseStatusCode,
      runReq,
      Option,
      POST(POST),
      Scheme(Https) ) 
import qualified Data.Text as T 
import qualified Data.ByteString.Lazy as BSL  
import Data.Aeson ( eitherDecode, encode )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Maybe ( fromJust ) 
import qualified Network.HTTP.Client.MultipartFormData as LM
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

paramToUrl :: PureStructs.Params -> FormUrlEncodedParam 
paramToUrl (PureStructs.ParamsText key val) = key =: val 
paramToUrl (PureStructs.ParamsNum key val) = key =: val 
paramToUrl (PureStructs.ParamsDouble key val) = key =: val 
paramToUrl (PureStructs.ParamsBool key val) = key =: val 
paramToUrl (PureStructs.ParamsTextList _ _) = mempty 
paramToUrl (PureStructs.ParamsJSON _ _) = mempty 

paramToMultipart :: PureStructs.Params -> [LM.Part] 
paramToMultipart (PureStructs.ParamsText key val) = [LM.partLBS key (encode val)]
paramToMultipart (PureStructs.ParamsNum key val) = [LM.partLBS key (encode val)]
paramToMultipart (PureStructs.ParamsDouble key val) = [LM.partLBS key (encode val)]
paramToMultipart (PureStructs.ParamsBool key val) = [LM.partLBS key (encode val)]
paramToMultipart (PureStructs.ParamsJSON key val) = [LM.partLBS key (encode val)]
paramToMultipart (PureStructs.ParamsTextList key val) = [LM.partLBS key (encode val)]

--paramToLBS :: PureStructs.Params -> BSL.ByteString 
--paramToLBS = encode 

paramsToUrlBody :: [PureStructs.Params] -> ReqBodyUrlEnc
paramsToUrlBody params = ReqBodyUrlEnc $ mconcat (paramToUrl <$> params)

paramsToMultipartBody :: [PureStructs.Params]-> IO ReqBodyMultipart
paramsToMultipartBody params = reqBodyMultipart $ mconcat (mapM paramToMultipart params)

isMultipart :: PureStructs.Params -> Bool 
isMultipart (PureStructs.ParamsJSON _ _) = True 
isMultipart (PureStructs.ParamsTextList _ _) = True 
isMultipart _ = False 

paramsToHttps :: PureStructs.Params -> Option Https 
paramsToHttps (PureStructs.ParamsText key val) = key =: val 
paramsToHttps (PureStructs.ParamsNum key val) = key =: val 
paramsToHttps _ = mempty

mbSendOption :: Config.BotType -> IO (Option Https)
mbSendOption vk@(Config.VKBot _) = do 
    params <-VKData.sendBasicParams vk    
    pure $ (mconcat (paramsToHttps <$> params))
mbSendOption _ = mempty

updateParam :: Config.BotType -> [PureStructs.Params] 
updateParam vk@(Config.VKBot _) = VKData.updateParams vk     
updateParam tel@(Config.TBot _) = TelData.updateParams tel 

data Method = Update | Send deriving Show 

mkHostPath :: Config.Config -> Method -> Maybe PureStructs.PureMessage-> Url Https
mkHostPath config Update _ = 
    case Config.botType config of     
        vk@(Config.VKBot _)-> hostPathToUrlScheme (fromJust $ VKData.updateHostPath vk) 
        tel@(Config.TBot _) -> hostPathToUrlScheme (fromJust $ TelData.updateHostPath tel) 
mkHostPath config Send (Just msg)= 
    case Config.botType config of 
        (Config.VKBot _)-> hostPathToUrlScheme VKData.sendHostPath 
        tel@(Config.TBot _) -> hostPathToUrlScheme $ fromJust (TelData.sendHostPath tel (PureStructs.messageType msg)) 

getPureMessageList :: Config.Config -> Logger.Logger -> IO (Either Logger.LogMessage [PureStructs.PureMessage])
getPureMessageList config logger = getU config >>= byteStringToPureMessageList config logger 

getU :: Config.Config ->  IO (Either Logger.LogMessage BSL.ByteString) 
getU config = do
    let params = (updateParam (Config.botType config)) 
    if any isMultipart params 
        then getUMultipartParams config params 
        else getUUrlParams config params 

getUUrlParams :: Config.Config -> [PureStructs.Params] -> IO (Either Logger.LogMessage BSL.ByteString) 
getUUrlParams config params = runReq defaultHttpConfig $ do     
        response <- req 
            POST
            (mkHostPath config Update Nothing)
            (paramsToUrlBody params)
            lbsResponse 
            mempty        
        (pure . pure) (responseBody response :: BSL.ByteString)  

getUMultipartParams :: Config.Config -> [PureStructs.Params] -> IO (Either Logger.LogMessage BSL.ByteString) 
getUMultipartParams config params = do 
        multiPartParams <- paramsToMultipartBody params 
        runReq defaultHttpConfig $ do     
            response <- req 
                POST
                (mkHostPath config Update Nothing)
                multiPartParams
                lbsResponse 
                mempty        
            (pure . pure) (responseBody response :: BSL.ByteString)  

sendM :: Config.Config -> Logger.Logger -> PureStructs.PureMessage -> IO (Either Logger.LogMessage Config.Config) 
sendM config logger basicMsg = do 
    eiMsg <- mbUploadFile (Config.botType config) logger basicMsg 
    case eiMsg of 
        Left err -> pure $ Left err 
        Right msg -> case PureStructs.mbParams msg of 
            Nothing -> pure $ Right config 
            Just params -> do     
                basicParams <- mbSendOption (Config.botType config)
                getApiResponse config basicParams params msg >>= checkApiResponse config logger msg              
    
getApiResponse :: Config.Config 
    -> Option Https
    -> [PureStructs.Params] 
    -> PureStructs.PureMessage 
    -> IO (Either Logger.LogMessage LbsResponse)  
getApiResponse config basicParams params msg = if any isMultipart params 
    then sendMMultipartParams config msg basicParams params
    else sendMUrlParams config msg basicParams params         
    
checkApiResponse :: Config.Config
    -> Logger.Logger 
    -> PureStructs.PureMessage 
    -> Either Logger.LogMessage LbsResponse
    -> IO (Either Logger.LogMessage Config.Config) 
checkApiResponse _ _ _ (Left err) = pure $ Left err 
checkApiResponse config logger msg (Right lbsResp) = case responseStatusCode lbsResp of 
    200 -> case Config.botType config of 
        (Config.VKBot _) -> do                            
            let sndMsgResult = eitherDecode (responseBody lbsResp) :: Either String VKStructs.VKResult 
            case sndMsgResult of 
                Left err -> pure $ Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld (T.pack err))
                Right (VKStructs.SendMsgError err) -> pure $ 
                    Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld (VKStructs.errMsg err))
                Right (VKStructs.SendMsgScs _) -> do 
                    liftIO $ Logger.botLog logger LoggerMsgs.sndMsgScsVK
                    pure $ pure (Config.configSetOffset config (PureStructs.updateID msg)) 
        (Config.TBot _) -> do 
            liftIO $ Logger.botLog logger LoggerMsgs.sndMsgScsTel
            pure $ Right (Config.configSetOffset config (succ (PureStructs.updateID msg))) 
    err -> return $ Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld ((T.pack . show) err))

sendMMultipartParams :: Config.Config 
    -> PureStructs.PureMessage
    -> Option Https
    -> [PureStructs.Params] 
    -> IO (Either Logger.LogMessage LbsResponse)  
sendMMultipartParams config msg basic params = do
    multiPartParams <- paramsToMultipartBody params 
    response <- runReq defaultHttpConfig $ do 
        req 
            POST 
            (mkHostPath config Send (Just msg))
            multiPartParams
            lbsResponse $
            basic 
    (pure . pure) response 
            
sendMUrlParams :: Config.Config 
    -> PureStructs.PureMessage
    -> Option Https
    -> [PureStructs.Params] 
    -> IO (Either Logger.LogMessage LbsResponse)  
sendMUrlParams config msg basic params = do
    response <- runReq defaultHttpConfig $ do 
        req 
            POST 
            (mkHostPath config Send (Just msg))
            (paramsToUrlBody params)
            lbsResponse $
            basic 
    (pure . pure) response 

byteStringToPureMessageList :: Config.Config -> Logger.Logger 
    -> Either Logger.LogMessage BSL.ByteString 
    -> IO (Either Logger.LogMessage [PureStructs.PureMessage])
byteStringToPureMessageList config@(Config.Config (Config.VKBot _)_ _ _ _) logger eiBS = 
    VKCleaners.vkByteStringToPureMessageList config logger eiBS 
byteStringToPureMessageList config@(Config.Config (Config.TBot _)_ _ _ _) logger eiBS = 
    TelCleaners.telByteStringToPureMessageList config logger eiBS 

mbUploadFile :: Config.BotType 
    -> Logger.Logger
    -> PureStructs.PureMessage 
    -> IO (Either Logger.LogMessage PureStructs.PureMessage)
mbUploadFile botType logger msg = case PureStructs.messageType msg of 
    PureStructs.MTCommon "Attachment" -> 
        if any isVKPhotoParam $ fromJust (PureStructs.mbParams msg)
            then makeUploadFileParams botType logger msg 
            else pure $ Right msg
    _ -> pure $ Right msg 

isVKPhotoParam ::  PureStructs.Params -> Bool 
isVKPhotoParam (PureStructs.ParamsText "VKPhotoUrl" _) = True 
isVKPhotoParam _ = False 

makeUploadFileParams :: Config.BotType
    -> Logger.Logger 
    -> PureStructs.PureMessage 
    -> IO (Either Logger.LogMessage PureStructs.PureMessage)
makeUploadFileParams = undefined