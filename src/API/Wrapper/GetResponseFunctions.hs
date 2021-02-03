module API.Wrapper.GetResponseFunctions where

import qualified API.Wrapper.Functions as WrapFunctions
import Control.Exception (handle)
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

getResponseMultipart ::
  Maybe (Url 'Https) ->
  [PureStructs.Params] ->
  Option 'Https ->
  IO LbsResponse
getResponseMultipart Nothing _ _ = BotEx.throwOtherException LoggerMsgs.invalidHP
getResponseMultipart (Just url) params options = do
  multipartParams <- WrapFunctions.paramsToMultipartBody params
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
getResponseUrl Nothing _ _ = BotEx.throwOtherException LoggerMsgs.invalidHP
getResponseUrl (Just ulr) params options =
  withHttpExceptionWrapped $
    runReq defaultHttpConfig $ do
      req
        POST
        ulr
        (WrapFunctions.paramsToUrlBody params)
        lbsResponse
        options

withHttpExceptionWrapped :: IO a -> IO a
withHttpExceptionWrapped = handle BotEx.throwHttpException
