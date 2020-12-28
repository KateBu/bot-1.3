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
import qualified API.WrapStructs as WrapStructs 

hostPathToUrlScheme :: Maybe WrapStructs.HostPath -> Maybe (Url 'Https) 
hostPathToUrlScheme (Just (WrapStructs.HostPath hpHost hpPath)) = pure $ makeUrlScheme (https hpHost) hpPath 
hostPathToUrlScheme _ = Nothing 

makeUrlScheme :: Url 'Https -> [T.Text] -> Url 'Https 
makeUrlScheme acc [] = acc
makeUrlScheme acc (x:xs) = makeUrlScheme (acc /: x) xs 

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

paramsToUrlBody :: [PureStructs.Params] -> ReqBodyUrlEnc
paramsToUrlBody params = ReqBodyUrlEnc $ mconcat (paramToUrl <$> params)

paramsToMultipartBody :: [PureStructs.Params]-> IO ReqBodyMultipart
paramsToMultipartBody params = reqBodyMultipart $ mconcat (mapM paramToMultipart params)

isMultipart :: PureStructs.Params -> Bool 
isMultipart (PureStructs.ParamsJSON _ _) = True 
isMultipart (PureStructs.ParamsTextList _ _) = True 
isMultipart _ = False 

paramsToHttps :: PureStructs.Params -> Option 'Https 
paramsToHttps (PureStructs.ParamsText key val) = key =: val 
paramsToHttps (PureStructs.ParamsNum key val) = key =: val 
paramsToHttps _ = mempty

mbSendOption :: Config.BotType -> IO (Option 'Https)
mbSendOption vk@(Config.VKBot _) = do 
    params <-VKData.sendBasicParams vk    
    pure $ (mconcat (paramsToHttps <$> params))
mbSendOption _ = mempty

updateParam :: Config.BotType -> [PureStructs.Params] 
updateParam vk@(Config.VKBot _) = VKData.updateParams vk     
updateParam tel@(Config.TBot _) = TelData.updateParams tel 

mkHostPath :: Config.Config -> WrapStructs.Method -> Maybe PureStructs.PureMessage-> Maybe (Url 'Https)
mkHostPath config WrapStructs.Update _ = 
    case Config.botType config of     
        vk@(Config.VKBot _)-> hostPathToUrlScheme (VKData.updateHostPath vk) 
        tel@(Config.TBot _) -> hostPathToUrlScheme (TelData.updateHostPath tel) 
mkHostPath config WrapStructs.Send (Just msg)= 
    case Config.botType config of 
        (Config.VKBot _)-> hostPathToUrlScheme VKData.sendHostPath 
        tel@(Config.TBot _) -> hostPathToUrlScheme $ (TelData.sendHostPath tel (PureStructs.messageType msg)) 
mkHostPath _ _ _ = Nothing 
 
getResponseUrl :: Maybe (Url 'Https)
    -> [PureStructs.Params]
    -> Option 'Https  
    -> IO (Either Logger.LogMessage LbsResponse) 
getResponseUrl Nothing _ _ = pure $ Left LoggerMsgs.invalidHP 
getResponseUrl (Just ulr) params options = runReq defaultHttpConfig $ do     
        response <- req 
            POST
            ulr
            (paramsToUrlBody params)
            lbsResponse 
            options 
        (pure . pure) response

getResponseMultipart :: Maybe (Url 'Https)
    -> [PureStructs.Params]
    -> Option 'Https  
    -> IO (Either Logger.LogMessage LbsResponse) 
getResponseMultipart Nothing _ _ = pure $ Left LoggerMsgs.invalidHP 
getResponseMultipart (Just url) params options = do 
    multipartParams <- paramsToMultipartBody params
    runReq defaultHttpConfig $ do      
        response <- req 
            POST
            url
            multipartParams
            lbsResponse 
            options 
        (pure . pure) response

responseToLbsByteString :: Either Logger.LogMessage LbsResponse
    -> IO (Either Logger.LogMessage BSL.ByteString) 
responseToLbsByteString (Left err) = pure $ Left err 
responseToLbsByteString (Right response) = case responseStatusCode response of 
    200 -> pure $ Right (responseBody response :: BSL.ByteString)
    err -> pure $ Left (Logger.makeLogMessage LoggerMsgs.badServerResponse ((T.pack . show) err))

getPureMessageList :: Config.Config -> Logger.Logger -> IO (Either Logger.LogMessage [PureStructs.PureMessage])
getPureMessageList config logger = getU config >>= byteStringToPureMessageList config logger 

getU :: Config.Config ->  IO (Either Logger.LogMessage BSL.ByteString) 
getU config = do
    let params = (updateParam (Config.botType config)) 
    let url = mkHostPath config WrapStructs.Update Nothing 
    if any isMultipart params 
        then getResponseMultipart url params mempty 
            >>= responseToLbsByteString 
        else getResponseUrl url params mempty 
            >>= responseToLbsByteString   

sendM :: Config.Config -> Logger.Logger -> PureStructs.PureMessage -> IO (Either Logger.LogMessage Config.Config) 
sendM config logger msg = case PureStructs.mbParams msg of 
            Nothing -> pure $ Right config 
            Just params -> do     
                basicParams <- mbSendOption (Config.botType config)
                getApiResponse config basicParams params msg >>= checkApiResponse config logger msg              
    
getApiResponse :: Config.Config 
    -> Option 'Https
    -> [PureStructs.Params] 
    -> PureStructs.PureMessage 
    -> IO (Either Logger.LogMessage LbsResponse)  
getApiResponse config basicParams params msg = do 
    let hostPath = mkHostPath config WrapStructs.Send (Just msg) 
    if any isMultipart params 
    then getResponseMultipart hostPath params basicParams  
    else getResponseUrl hostPath params basicParams         
    
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
                Right (VKStructs.SendMsgError (VKStructs.SendError err)) -> pure $ 
                    Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld (VKStructs.errMsg err))
                Right (VKStructs.SendMsgScs _) -> do 
                    liftIO $ Logger.botLog logger LoggerMsgs.sndMsgScsVK
                    pure $ pure (Config.configSetOffset config (PureStructs.updateID msg)) 
        (Config.TBot _) -> do 
            liftIO $ Logger.botLog logger LoggerMsgs.sndMsgScsTel
            pure $ Right (Config.configSetOffset config (succ (PureStructs.updateID msg))) 
    err -> return $ Left (Logger.makeLogMessage LoggerMsgs.sndMsgFld ((T.pack . show) err))

byteStringToPureMessageList :: Config.Config -> Logger.Logger 
    -> Either Logger.LogMessage BSL.ByteString 
    -> IO (Either Logger.LogMessage [PureStructs.PureMessage])
byteStringToPureMessageList config@(Config.Config (Config.VKBot _)_ _ _ _) logger eiBS = 
    VKCleaners.vkByteStringToPureMessageList config logger eiBS 
byteStringToPureMessageList config@(Config.Config (Config.TBot _)_ _ _ _) logger eiBS = 
    TelCleaners.telByteStringToPureMessageList config logger eiBS 
 