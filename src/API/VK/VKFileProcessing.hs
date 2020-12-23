module API.VK.VKFileProcessing where

import Network.HTTP.Req
import qualified Data.Text as T 
import qualified Data.ByteString.Lazy as BSL  
import Data.Aeson 
import qualified API.VK.VKData as VKData 
import qualified API.VK.Structs as VKStructs 
import qualified API.Wrapper as Wrapper 
import qualified Config.Config as Config 
import qualified Logger.Logger as Logger 
import qualified Logger.LoggerMsgs as LoggerMsgs 
import qualified Logic.PureStructs as PureStructs

downloadedPhotoToBody :: [VKStructs.PhSizes] -> IO (Either Logger.LogMessage ReqBodyMultipart) 
downloadedPhotoToBody sizes = getSmallImageUrl sizes 
    >>= getImageServerResponse 
    >>= getImage  
    >>= makePhotoParams 
    >>= makePhotoRequestBody 

getSmallImageUrl :: [VKStructs.PhSizes] -> IO (Either Logger.LogMessage T.Text)
getSmallImageUrl [] = pure $ Left LoggerMsgs.noImgSize 
getSmallImageUrl (VKStructs.PhSizes url _ : _) = pure $ Right url 
--getSmallImageUrl (_ : sizes) = getSmallImageUrl sizes 

getImageServerResponse :: Either Logger.LogMessage T.Text -> IO (Either Logger.LogMessage LbsResponse)
getImageServerResponse (Left err) = pure $ Left err 
getImageServerResponse (Right url) = case VKData.urlToHostPath url of 
    Nothing -> pure $ Left LoggerMsgs.noHostPath 
    Just hostPath -> case VKData.urlToParams url of 
        Nothing -> pure $ Left LoggerMsgs.noParams 
        Just params -> do 
            response <- runReq defaultHttpConfig $ do 
                req 
                    POST 
                    (Wrapper.hostPathToUrlScheme hostPath)
                    NoReqBody 
                    lbsResponse
                    (mconcat $ Wrapper.paramsToHttps <$> params)
            (pure . pure) response

getImage :: Either Logger.LogMessage LbsResponse 
    -> IO (Either Logger.LogMessage BSL.ByteString)  
getImage (Left err) = pure $ Left err 
getImage (Right response) = case responseStatusCode response of 
    200 -> (pure . pure) (responseBody response :: BSL.ByteString) 
    err -> pure $ Left (Logger.makeLogMessage LoggerMsgs.getUploadServerErr $ (T.pack . show) err)

makePhotoParams :: Either Logger.LogMessage BSL.ByteString 
    -> IO (Either Logger.LogMessage PureStructs.Params)
makePhotoParams (Left err) = pure $ Left err 
makePhotoParams (Right photo) = pure $ 
    Right (PureStructs.ParamsFile "photo" photo)

makePhotoRequestBody :: Either Logger.LogMessage PureStructs.Params
    -> IO (Either Logger.LogMessage ReqBodyMultipart)
makePhotoRequestBody (Left err) = pure $ Left err 
makePhotoRequestBody (Right params) = Right <$> Wrapper.paramsToMultipartBody [params]

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
    case VKData.urlToHostPath url of 
        Nothing -> pure $ Left LoggerMsgs.noHostPath 
        Just hostPath -> case VKData.urlToParams url of 
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

makeVKUploadPhotoServerResponse :: Either Logger.LogMessage BSL.ByteString 
    -> IO (Either Logger.LogMessage VKStructs.VKUploadPhotoServerResponse)
makeVKUploadPhotoServerResponse (Left err) = pure $ Left err
makeVKUploadPhotoServerResponse (Right response) = do 
    case eitherDecode response :: Either String VKStructs.VKUploadPhotoServerResponse of 
        Left err -> pure $ Left (Logger.makeLogMessage LoggerMsgs.makeVKUploadPhotoServerResponseErr (T.pack err))
        Right decoded -> case decoded of 
            VKStructs.PhotoServer ps -> pure $ Right (VKStructs.PhotoServer ps)
            VKStructs.PhotoServerError (VKStructs.VKResultError _ err_msg) -> 
                pure $ Left (Logger.makeLogMessage LoggerMsgs.makeVKUploadPhotoServerResponseErr err_msg)
    
uploadPhoto :: [VKStructs.PhSizes] 
    -> Config.BotType 
    -> IO (Either Logger.LogMessage LbsResponse)   
uploadPhoto sizes botType = do 
    eiPhotoBody <- downloadedPhotoToBody sizes 
    case eiPhotoBody of 
        Left err -> pure $ Left err 
        Right photoBody -> do 
            eiServer <- getUploadServerHostPathParams botType 
            case eiServer of 
                Left err -> pure $ Left err 
                Right (hostPath, params) -> do 
                    response <- runReq defaultHttpConfig $ do 
                        req 
                            POST 
                            (Wrapper.hostPathToUrlScheme hostPath)
                            photoBody 
                            lbsResponse
                            (mconcat $ Wrapper.paramsToHttps <$> params)
                    (pure . pure) response 

uploadPhotoResponse :: Either Logger.LogMessage LbsResponse -> 
    IO (Either Logger.LogMessage VKStructs.UploadPhotoResponse)
uploadPhotoResponse (Left err) = pure $ Left err 
uploadPhotoResponse (Right lbsResp) = 
    case responseStatusCode lbsResp of 
        200 -> case eitherDecode (responseBody lbsResp) :: Either String VKStructs.UploadPhotoResponse of 
            Left err -> pure $ Left (Logger.makeLogMessage LoggerMsgs.parseUplPhotoFld (T.pack err)) 
            Right (VKStructs.UploadPhotoError _) -> pure $ Left LoggerMsgs.uplPhotoRespErr
            Right resp -> pure $ Right resp 
        err -> pure $ Left (Logger.makeLogMessage LoggerMsgs.uplPhotoRespErr $ (T.pack . show) err)

makeSavePhotoParams :: Either Logger.LogMessage VKStructs.UploadPhotoResponse
    -> IO (Either Logger.LogMessage [PureStructs.Params]) 
makeSavePhotoParams (Left err) = pure $ Left err 
makeSavePhotoParams (Right (VKStructs.UploadPhotoResponse serv photo hash)) = pure $ 
    Right [PureStructs.ParamsNum "server" serv
        , PureStructs.ParamsText "hash" hash
        , PureStructs.ParamsText "photo" photo]

savePhotoRequest :: Either Logger.LogMessage [PureStructs.Params] 
    -> IO (Either Logger.LogMessage BSL.ByteString)
savePhotoRequest (Left err) = pure $ Left err 
savePhotoRequest (Right params) = do 
    response <- runReq defaultHttpConfig $ do 
        req 
            POST 
            (Wrapper.hostPathToUrlScheme VKData.savePhotoServerHostPath)
            NoReqBody 
            lbsResponse
            (mconcat $ Wrapper.paramsToHttps <$> params)
    case responseStatusCode response of 
        200 -> (pure . pure) (responseBody response :: BSL.ByteString) 
        err -> pure $ Left (Logger.makeLogMessage LoggerMsgs.savePhotoRequestErr $ (T.pack . show) err)

makeUploadedPhotoParams :: Either Logger.LogMessage BSL.ByteString 
    -> IO (Either Logger.LogMessage [PureStructs.Params])
makeUploadedPhotoParams (Left err) = pure $ Left err 
makeUploadedPhotoParams (Right lbs) = undefined


