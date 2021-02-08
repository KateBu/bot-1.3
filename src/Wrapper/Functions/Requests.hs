module Wrapper.Functions.Requests where

import Control.Exception (handle)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Environment.Exports as Env
import qualified Environment.Logger.Exports as Logger
import qualified Exceptions.Exports as BotEx
import qualified Logic.PureStructs as PureStructs
import Network.HTTP.Req
  ( LbsResponse,
    Option,
    POST (POST),
    Scheme (Https),
    Url,
    defaultHttpConfig,
    lbsResponse,
    req,
    runReq,
  )
import qualified TextMessages.LoggerMessages as LoggerMsgs
import qualified Wrapper.Functions.URL as WrapFunctions
import qualified Wrapper.Structs as WrapStructs

sendMessageRequest ::
  Env.Environment IO ->
  Option 'Https ->
  [PureStructs.Params] ->
  PureStructs.PureMessage ->
  IO LbsResponse
sendMessageRequest env basicParams params msg = do
  logger <- runReaderT Env.eLogger env
  config <- runReaderT Env.eConfig env
  Logger.botLog logger LoggerMsgs.getApiResponseInProgress
  let hostPath = WrapFunctions.buildHostPath config WrapStructs.Send (Just msg)
  if any WrapFunctions.isMultipart params
    then getResponseMultipart hostPath params basicParams
    else getResponseUrl hostPath params basicParams

getUpdatesRequest :: Env.Environment IO -> IO LbsResponse
getUpdatesRequest env = do
  config <- runReaderT Env.eConfig env
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.getUpdatesInProcess
  let params = WrapFunctions.updateParam config
  let url = WrapFunctions.buildHostPath config WrapStructs.GetUpdate Nothing
  if any WrapFunctions.isMultipart params
    then do
      Logger.botLog logger logMsgMultipart
      getResponseMultipart url params mempty
    else do
      Logger.botLog logger logMsgUrl
      getResponseUrl url params mempty
  where
    logMsgMultipart = LoggerMsgs.getResponseMultipartInProgress
    logMsgUrl = LoggerMsgs.getResponseUrlInProgress

getResponseMultipart ::
  Maybe (Url 'Https) ->
  [PureStructs.Params] ->
  Option 'Https ->
  IO LbsResponse
getResponseMultipart Nothing _ _ = BotEx.throwOtherException LoggerMsgs.invalidHostPath
getResponseMultipart (Just url) params options = do
  multipartParams <- WrapFunctions.buildMultipartBody params
  withHttpExceptionWrapped $
    runReq defaultHttpConfig $ do
      req
        POST
        url
        multipartParams
        lbsResponse
        options

getResponseUrl ::
  Maybe (Url 'Https) ->
  [PureStructs.Params] ->
  Option 'Https ->
  IO LbsResponse
getResponseUrl Nothing _ _ = BotEx.throwOtherException LoggerMsgs.invalidHostPath
getResponseUrl (Just ulr) params options =
  withHttpExceptionWrapped $
    runReq defaultHttpConfig $ do
      req
        POST
        ulr
        (WrapFunctions.buildUrlBody params)
        lbsResponse
        options

withHttpExceptionWrapped :: IO a -> IO a
withHttpExceptionWrapped = handle BotEx.throwHttpException
