module API.Wrapper where

import Network.HTTP.Req
    ( LbsResponse,
      Url,
      FormUrlEncodedParam,
      ReqBodyMultipart,
      ReqBodyUrlEnc(..),
      NoReqBody(..),
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

urlToHostPath :: T.Text -> Maybe PureStructs.HostPath 
urlToHostPath url = 
    let 
        hostPath = T.drop 3 (T.dropWhile (/= ':') $ T.takeWhile (/= '?') url)
        (host,rest) = T.span ( /= '/') hostPath 
        path = filter (/= "") (T.splitOn "/" rest)
    in pure $ PureStructs.HostPath host path 

urlToParams :: VKStructs.Url -> Maybe [PureStructs.Params] 
urlToParams url = let 
    params = T.drop 1 $ T.dropWhile (/='?') url 
    keyVals = filter (/="") $ T.splitOn "&" params 
    keys = T.splitOn "=" <$> keyVals   
    in mapM listToParams keys  

listToParams :: [T.Text] -> Maybe PureStructs.Params 
listToParams (key : val : []) = Just $ PureStructs.ParamsText key val 
listToParams _ = Nothing 

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
        vk@(Config.VKBot _)-> hostPathToUrlScheme (VKData.updateHostPath vk) 
        tel@(Config.TBot _) -> hostPathToUrlScheme (TelData.updateHostPath tel) 
mkHostPath config Send (Just msg)= 
    case Config.botType config of 
        (Config.VKBot _)-> hostPathToUrlScheme VKData.sendHostPath 
        tel@(Config.TBot _) -> hostPathToUrlScheme $ (TelData.sendHostPath tel (PureStructs.messageType msg)) 

getResponseUrl :: Url Https
    -> [PureStructs.Params]
    -> Option Https  
    -> IO (Either Logger.LogMessage LbsResponse) 
getResponseUrl ulr params options = runReq defaultHttpConfig $ do     
        response <- req 
            POST
            ulr
            (paramsToUrlBody params)
            lbsResponse 
            options 
        (pure . pure) response

getResponseMultipart :: Url Https
    -> [PureStructs.Params]
    -> Option Https  
    -> IO (Either Logger.LogMessage LbsResponse) 
getResponseMultipart url params options = do 
    multipartParams <- paramsToMultipartBody params
    runReq defaultHttpConfig $ do      
        response <- req 
            POST
            url
            multipartParams
            lbsResponse 
            options 
        (pure . pure) response

getResponseNoBody :: Url Https
    -> Option Https  
    -> IO (Either Logger.LogMessage LbsResponse) 
getResponseNoBody url options = do 
    runReq defaultHttpConfig $ do      
        response <- req 
            POST
            url
            NoReqBody
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
    let url = mkHostPath config Update Nothing 
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
    -> Option Https
    -> [PureStructs.Params] 
    -> PureStructs.PureMessage 
    -> IO (Either Logger.LogMessage LbsResponse)  
getApiResponse config basicParams params msg = do 
    let hostPath = mkHostPath config Send (Just msg) 
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
                Right (VKStructs.SendMsgError err) -> pure $ 
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

{-


isAttachmentParam :: PureStructs.Params -> Bool 
isAttachmentParam (PureStructs.ParamsText "attachment" _) = True 
isAttachmentParam _ = False 

isVKPhotoParam ::  PureStructs.Params -> Bool 
isVKPhotoParam (PureStructs.ParamsText "VKPhotoUrl" _) = True 
isVKPhotoParam _ = False 

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

makeUploadFileParams :: Config.BotType 
    -> Logger.Logger 
    -> PureStructs.PureMessage 
    -> IO (Either Logger.LogMessage PureStructs.PureMessage)
makeUploadFileParams botType logger msg = do 
    Logger.botLog logger LoggerMsgs.needUpload 
    eiParams <- makeParams botType logger (PureStructs.mbParams msg) 
    case eiParams of 
        Left err -> pure $ Left err 
        Right params -> pure $ Right msg {PureStructs.mbParams = params}

makeParams :: Config.BotType 
    -> Logger.Logger 
    -> Maybe [PureStructs.Params] 
    -> IO (Either Logger.LogMessage (Maybe [PureStructs.Params]))
makeParams _ _ Nothing = pure $ Left LoggerMsgs.noParams
makeParams botType logger (Just params) = do 
    let atParams = filter isAttachmentParam params 
    let phParams = filter isVKPhotoParam params 
    let otherParams = filter (not . isAttachmentParam) $ filter (not . isVKPhotoParam) params 
    eiNewParams <- makeAttachmentParams botType logger atParams phParams
    case eiNewParams of 
        Left err -> pure $ Left err 
        Right newParams -> pure $ Right (Just otherParams <> Just newParams)

makeAttachmentParams :: Config.BotType 
    -> Logger.Logger 
    -> [PureStructs.Params] 
    -> [PureStructs.Params] 
    -> IO (Either Logger.LogMessage [PureStructs.Params])
makeAttachmentParams botType logger atParam phParam = do 
    Logger.botLog logger LoggerMsgs.mkNewParam 
    eiPhotoInfo <- setUploadedPhotoInfo botType logger phParam 
    case eiPhotoInfo of 
        Left err -> pure $ Left err 
        Right phInfo -> case atParam of 
            [] -> pure $ Right [PureStructs.ParamsText "attachment" phInfo ]
            [PureStructs.ParamsText "attachment" attachments] -> pure $ 
                Right [PureStructs.ParamsText "attachment" (attachments <> "," <> phInfo)]
            _ -> pure $ Left LoggerMsgs.unexpAttachment 

setUploadedPhotoInfo :: Config.BotType 
    -> Logger.Logger 
    -> [PureStructs.Params] 
    -> IO (Either Logger.LogMessage T.Text)
setUploadedPhotoInfo botType logger params = do 
    eiPhotoParams <- getPhotosInfo botType logger params 
    case eiPhotoParams of 
        Left err -> pure $ Left err 
        Right phParams -> pure $ Right (T.intercalate "," phParams)

getPhotosInfo :: Config.BotType 
    -> Logger.Logger 
    -> [PureStructs.Params] 
    -> IO (Either Logger.LogMessage [T.Text])
getPhotosInfo botType logger params = do 
    eiPhotoInfo <- mapM (getPhotoInfo botType logger) params 
    case sequence eiPhotoInfo of 
        Left err -> pure $ Left err 
        Right phInfo -> pure $ Right phInfo 

getPhotoInfo :: Config.BotType 
    -> Logger.Logger 
    -> PureStructs.Params 
    -> IO (Either Logger.LogMessage T.Text)
getPhotoInfo botType logger param = do 
    Logger.botLog logger LoggerMsgs.getPhotoInfoIsRunning
    param <- makePhotoInfo botType logger param 
    case param of 
        Left err -> pure $ Left err 
        Right (VKStructs.VKPhoto itemId ownerId _) -> 
            pure $ Right ("photo" 
                <> (T.pack . show) ownerId 
                <> "_"
                <> (T.pack . show) itemId)
        _ -> pure $ Left LoggerMsgs.unexpObject 

makePhotoInfo :: Config.BotType 
    -> Logger.Logger 
    -> PureStructs.Params 
    -> IO (Either Logger.LogMessage VKStructs.AObject)
makePhotoInfo botType logger (PureStructs.ParamsText "VKPhotoUrl" url) = undefined 

downloadedPhotoParams :: Logger.Logger -> T.Text -> IO (Either Logger.LogMessage [PureStructs.Params])
downloadedPhotoParams logger url = do 
    Logger.botLog logger LoggerMsgs.mkPhInfo 
    downloadPhotoLbs url >>= photoLbsToParam 

getUploadServerHostPathParams :: Config.BotType 
    -> Logger.Logger
    -> IO (Either Logger.LogMessage (PureStructs.HostPath, [PureStructs.Params]))
getUploadServerHostPathParams botType logger = getServerToUploadUrl botType logger
    >>= makeVKUploadPhotoServerResponse logger
    >>= uploadServerHostPathParams logger 

uploadPhotoToServer :: Config.BotType
    -> Logger.Logger 
    -> T.Text
    -> IO (Either Logger.LogMessage BSL.ByteString)  
uploadPhotoToServer botType logger url = do 
    photoParams <- downloadedPhotoParams logger url 
    server <- getUploadServerHostPathParams botType logger 
    tryToUpload logger server photoParams >>= responseToLbsByteString 

tryToUpload :: Logger.Logger 
    -> Either Logger.LogMessage (PureStructs.HostPath, [PureStructs.Params])
    -> Either Logger.LogMessage [PureStructs.Params] 
    -> IO (Either Logger.LogMessage LbsResponse)
tryToUpload _ (Left err) _ = pure $ Left err 
tryToUpload _ _ (Left err) = pure $ Left err 



{-
getUploadServerHostPathParams :: Config.BotType 
    -> IO (Either Logger.LogMessage (PureStructs.HostPath, [PureStructs.Params]))
getUploadServerHostPathParams botType = getUploadServer botType 
    >>= makeVKUploadPhotoServerResponse 
    >>= uploadServerHostPathParams

uploadServerHostPathParams :: Either Logger.LogMessage VKStructs.VKUploadPhotoServerResponse
    -> IO (Either Logger.LogMessage (PureStructs.HostPath, [PureStructs.Params])) 
uploadServerHostPathParams (Left err) = pure $ Left err 
uploadServerHostPathParams (Right (VKStructs.PhotoServerError _)) = pure $ Left LoggerMsgs.noHostPath 
uploadServerHostPathParams (Right (VKStructs.PhotoServer (VKStructs.UploadUrl url))) = 
    case Wrapper.urlToHostPath url of 
        Nothing -> pure $ Left LoggerMsgs.noHostPath 
        Just hostPath -> case Wrapper.urlToParams url of 
            Nothing -> pure $ Left LoggerMsgs.noParams 
            Just params -> pure $ Right (hostPath, params)

getUploadServer :: Config.BotType -> IO (Either Logger.LogMessage BSL.ByteString)  
getUploadServer bot = do 
    response <- runReq defaultHttpConfig $ do 
        req 
            POST 
            (Wrapper.hostPathToUrlScheme VKData.uploadPhotoServerHostPath)
            NoReqBody 
            lbsResponse
            (mconcat $ Wrapper.paramsToHttps <$> VKData.getUploadFileParams bot)
    case responseStatusCode response of 
        200 -> (pure . pure) (responseBody response :: BSL.ByteString) 
        err -> pure $ Left (Logger.makeLogMessage LoggerMsgs.getUploadServerErr $ (T.pack . show) err)
-}
uploadServerHostPathParams :: Logger.Logger
    ->  Either Logger.LogMessage VKStructs.VKUploadPhotoServerResponse
    -> IO (Either Logger.LogMessage (PureStructs.HostPath, [PureStructs.Params])) 
uploadServerHostPathParams _ (Left err) = pure $ Left err 
uploadServerHostPathParams logger (Right (VKStructs.PhotoServerError _)) = pure $ Left LoggerMsgs.noHostPath 
uploadServerHostPathParams logger (Right (VKStructs.PhotoServer (VKStructs.UploadUrl url))) = 
    case urlToHostPath url of 
        Nothing -> pure $ Left LoggerMsgs.noHostPath 
        Just hostPath -> case urlToParams url of 
            Nothing -> pure $ Left LoggerMsgs.noParams 
            Just params -> pure $ Right (hostPath, params)

makeVKUploadPhotoServerResponse :: Logger.Logger 
    -> Either Logger.LogMessage BSL.ByteString 
    -> IO (Either Logger.LogMessage VKStructs.VKUploadPhotoServerResponse)
makeVKUploadPhotoServerResponse _ (Left err) = pure $ Left err
makeVKUploadPhotoServerResponse logger (Right response) = do 
    case eitherDecode response :: Either String VKStructs.VKUploadPhotoServerResponse of 
        Left err -> pure $ Left (Logger.makeLogMessage LoggerMsgs.makeVKUploadPhotoServerResponseErr (T.pack err))
        Right decoded -> case decoded of 
            VKStructs.PhotoServer ps -> pure $ Right (VKStructs.PhotoServer ps)
            VKStructs.PhotoServerError (VKStructs.VKResultError _ err_msg) -> 
                pure $ Left (Logger.makeLogMessage LoggerMsgs.makeVKUploadPhotoServerResponseErr err_msg)


getServerToUploadUrl :: Config.BotType 
    -> Logger.Logger 
    -> IO (Either Logger.LogMessage BSL.ByteString)  
getServerToUploadUrl botType logger = do 
    Logger.botLog logger LoggerMsgs.getServUpl
    getResponseNoBody 
        (hostPathToUrlScheme VKData.uploadPhotoServerHostPath)
        (mconcat $ paramsToHttps <$> VKData.getUploadFileParams botType)
    >>= responseToLbsByteString 

downloadPhotoLbs :: T.Text -> IO (Either Logger.LogMessage BSL.ByteString)
downloadPhotoLbs url = do 
    http <- parseRequest $ T.unpack url 
    updRequest <- httpLBS http
    let respBody = getResponseBody updRequest 
    pure $ Right respBody 
            
photoLbsToParam :: Either Logger.LogMessage BSL.ByteString
    -> IO (Either Logger.LogMessage [PureStructs.Params])
photoLbsToParam (Left err) = pure $ Left err 
photoLbsToParam (Right photoLbs) = pure $ Right 
    [PureStructs.ParamsFile "photo" photoLbs]

-}