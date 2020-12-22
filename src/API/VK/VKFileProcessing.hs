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

getUploadServer :: Config.BotType -> IO (Either Logger.LogMessage LbsResponse)  
getUploadServer bot = do 
    response <- runReq defaultHttpConfig $ do 
        req 
            POST 
            (Wrapper.hostPathToUrlScheme VKData.getUploadPhotoServerHostPath)
            NoReqBody 
            lbsResponse
            (mconcat $ Wrapper.paramsToHttps <$> VKData.getUploadFileParams bot)
    (pure . pure) response 
    
