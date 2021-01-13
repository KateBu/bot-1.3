module API.Wrapper.GetResponseFunctions where

import qualified API.Wrapper.WrapFunctions as WrapFunctions
import Control.Exception (catch)
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs
import Network.HTTP.Req
  ( HttpException,
    LbsResponse,
    Option,
    POST (POST),
    Scheme (Https),
    Url,
    defaultHttpConfig,
    lbsResponse,
    req,
    runReq,
  )

getResponseMultipart ::
  Maybe (Url 'Https) ->
  [PureStructs.Params] ->
  Option 'Https ->
  IO LbsResponse
getResponseMultipart Nothing _ _ = BotEx.throwOtherException LoggerMsgs.invalidHP
getResponseMultipart (Just url) params options = do
  multipartParams <- WrapFunctions.paramsToMultipartBody params
  catch
    ( runReq defaultHttpConfig $ do
        req
          POST
          url
          multipartParams
          lbsResponse
          options
    )
    (\ex -> BotEx.throwHttpException (ex :: HttpException))

getResponseUrl ::
  Maybe (Url 'Https) ->
  [PureStructs.Params] ->
  Option 'Https ->
  IO LbsResponse
getResponseUrl Nothing _ _ = BotEx.throwOtherException LoggerMsgs.invalidHP
getResponseUrl (Just ulr) params options =
  catch
    ( runReq defaultHttpConfig $ do
        req
          POST
          ulr
          (WrapFunctions.paramsToUrlBody params)
          lbsResponse
          options
    )
    (\ex -> BotEx.throwHttpException (ex :: HttpException))
