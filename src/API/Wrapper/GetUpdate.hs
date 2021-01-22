module API.Wrapper.GetUpdate where

import qualified API.Telegram.Cleaners as TelCleaners
import qualified API.VK.Cleaners.ToPureMsgList as VKCleaners
import API.Wrapper.GetResponseFunctions
  ( getResponseMultipart,
    getResponseUrl,
  )
import qualified API.Wrapper.WrapFunctions as WrapFunctions
import qualified API.Wrapper.WrapStructs as WrapStructs
import qualified Config.Config as Config
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Environment as Env
import qualified Environment.Logger.Logger as Logger
import qualified Environment.Logger.LoggerMsgs as LoggerMsgs
import qualified Exceptions.Exceptions as BotEx
import qualified Logic.PureStructs as PureStructs
import Network.HTTP.Req
  ( LbsResponse,
    responseBody,
    responseStatusCode,
  )

getPureMessageList :: Env.Environment IO -> IO [PureStructs.PureMessage]
getPureMessageList env = getU env >>= byteStringToPureMessageList env

getU :: Env.Environment IO -> IO BSL.ByteString
getU env = do
  config <- runReaderT Env.eConfig env
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.getUpdInProcess
  let params = WrapFunctions.updateParam config
  let url = WrapFunctions.mkHostPath config WrapStructs.Update Nothing
  lbsResp <-
    if any WrapFunctions.isMultipart params
      then do
        Logger.botLog logger LoggerMsgs.getRespMulti
        getResponseMultipart url params mempty
      else do
        Logger.botLog logger LoggerMsgs.getRespUrl
        getResponseUrl url params mempty
  responseToLbsByteString lbsResp

responseToLbsByteString ::
  LbsResponse ->
  IO BSL.ByteString
responseToLbsByteString response = case responseStatusCode response of
  200 -> pure (responseBody response :: BSL.ByteString)
  err ->
    BotEx.throwOtherException
      (Logger.makeLogMessage LoggerMsgs.badServerResponse ((T.pack . show) err))

byteStringToPureMessageList ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
byteStringToPureMessageList env bs = do
  currentConfig <- runReaderT Env.eConfig env
  case currentConfig of
    (Config.VKBot _) ->
      VKCleaners.vkByteStringToPureMessageList env bs
    (Config.TBot _) ->
      TelCleaners.telByteStringToPureMessageList env bs
