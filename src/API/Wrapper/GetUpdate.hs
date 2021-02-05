module API.Wrapper.GetUpdate where

import qualified API.Telegram.Cleaners as TelCleaners
import qualified API.VK.Cleaners.ToPureMsgList as VKCleaners
import qualified API.Wrapper.Functions as WrapFunctions
import API.Wrapper.GetResponseFunctions
  ( getResponseMultipart,
    getResponseUrl,
  )
import qualified API.Wrapper.Structs as WrapStructs
import qualified Config.Exports as Config
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Environment.Logger.Exports as Logger
import qualified Exceptions.Exports as BotEx
import qualified Logic.PureStructs as PureStructs
import Network.HTTP.Req
  ( LbsResponse,
    responseBody,
    responseStatusCode,
  )
import qualified TextMessages.LoggerMessages as LoggerMsgs

getPureMessageList :: Env.Environment IO -> IO [PureStructs.PureMessage]
getPureMessageList env = do
  updates <- getUpdates env
  byteStringToPureMessageList env updates

getUpdates :: Env.Environment IO -> IO BSL.ByteString
getUpdates env = do
  config <- runReaderT Env.eConfig env
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.getUpdatesInProcess
  let params = WrapFunctions.updateParam config
  let url = WrapFunctions.makeHostPath config WrapStructs.Update Nothing
  lbsResp <-
    if any WrapFunctions.isMultipart params
      then do
        Logger.botLog logger logMsgMultipart
        getResponseMultipart url params mempty
      else do
        Logger.botLog logger logMsgUrl
        getResponseUrl url params mempty
  responseToLbsByteString lbsResp
  where
    logMsgMultipart = LoggerMsgs.getResponseMultipartInProgress
    logMsgUrl = LoggerMsgs.getResponseUrlInProgress

responseToLbsByteString ::
  LbsResponse ->
  IO BSL.ByteString
responseToLbsByteString response = case responseStatusCode response of
  200 -> pure (responseBody response :: BSL.ByteString)
  err -> BotEx.throwOtherException logMsg
    where
      logMsg = Logger.makeLogMessage LoggerMsgs.badServerResponse ((T.pack . show) err)

byteStringToPureMessageList ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
byteStringToPureMessageList env bytestring = do
  config <- runReaderT Env.eConfig env
  case config of
    (Config.VKBot _) ->
      VKCleaners.vkByteStringToPureMessageList env bytestring
    (Config.TBot _) ->
      TelCleaners.telByteStringToPureMessageList env bytestring
