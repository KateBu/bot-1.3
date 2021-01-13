module API.Wrapper where

import qualified API.Telegram.Cleaners as TelCleaners
import qualified API.VK.Cleaners.ToPureMsgList as VKCleaners
import qualified API.VK.Structs as VKStructs
import qualified API.WrapFunctions as WrapFunctions
import qualified API.WrapStructs as WrapStructs
import qualified Config.Config as Config
import Control.Exception (catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.Logger as Logger
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
    responseBody,
    responseStatusCode,
    runReq,
  )

getPureMessageList :: Config.Config -> Logger.Logger -> IO [PureStructs.PureMessage]
getPureMessageList config logger = getU config >>= byteStringToPureMessageList config logger

getU :: Config.Config -> IO BSL.ByteString
getU config = do
  let params = WrapFunctions.updateParam (Config.botType config)
  let url = WrapFunctions.mkHostPath config WrapStructs.Update Nothing
  lbsResp <-
    if any WrapFunctions.isMultipart params
      then getResponseMultipart url params mempty
      else getResponseUrl url params mempty
  responseToLbsByteString lbsResp

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

responseToLbsByteString ::
  LbsResponse ->
  IO BSL.ByteString
responseToLbsByteString response = case responseStatusCode response of
  200 -> pure (responseBody response :: BSL.ByteString)
  err ->
    BotEx.throwOtherException
      (Logger.makeLogMessage LoggerMsgs.badServerResponse ((T.pack . show) err))

byteStringToPureMessageList ::
  Config.Config ->
  Logger.Logger ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
byteStringToPureMessageList config@(Config.Config (Config.VKBot _) _ _ _ _) logger eiBS =
  VKCleaners.vkByteStringToPureMessageList config logger eiBS
byteStringToPureMessageList config@(Config.Config (Config.TBot _) _ _ _ _) logger eiBS =
  TelCleaners.telByteStringToPureMessageList config logger eiBS

sendM ::
  Config.Config ->
  Logger.Logger ->
  PureStructs.PureMessage ->
  IO Config.Config
sendM config logger msg = do
  let mbParams = PureStructs.mbParams msg
  maybe (pure config) (mkSendConfig config logger msg) mbParams

mkSendConfig ::
  Config.Config ->
  Logger.Logger ->
  PureStructs.PureMessage ->
  [PureStructs.Params] ->
  IO Config.Config
mkSendConfig config logger msg params = do
  basicParams <- WrapFunctions.mbSendOption (Config.botType config)
  getApiResponse config basicParams params msg >>= checkApiResponse config logger msg

getApiResponse ::
  Config.Config ->
  Option 'Https ->
  [PureStructs.Params] ->
  PureStructs.PureMessage ->
  IO LbsResponse
getApiResponse config basicParams params msg = do
  let hostPath = WrapFunctions.mkHostPath config WrapStructs.Send (Just msg)
  if any WrapFunctions.isMultipart params
    then getResponseMultipart hostPath params basicParams
    else getResponseUrl hostPath params basicParams

checkApiResponse ::
  Config.Config ->
  Logger.Logger ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO Config.Config
checkApiResponse config logger msg lbsResp = case responseStatusCode lbsResp of
  200 -> newBotConfig config (Config.botType config) logger msg lbsResp
  err -> BotEx.throwSendExcept $ Logger.makeLogMessage LoggerMsgs.sndMsgFld ((T.pack . show) err)

newBotConfig ::
  Config.Config ->
  Config.BotType ->
  Logger.Logger ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO Config.Config
newBotConfig config (Config.VKBot _) logger msg lbsResp = do
  let sndMsgResult = eitherDecode (responseBody lbsResp) :: Either String VKStructs.VKResult
  checkResult config logger msg sndMsgResult
newBotConfig config (Config.TBot _) logger msg _ = do
  liftIO $ Logger.botLog logger LoggerMsgs.sndMsgScsTel
  pure (Config.configSetOffset config (succ (PureStructs.updateID msg)))

checkResult ::
  Config.Config ->
  Logger.Logger ->
  PureStructs.PureMessage ->
  Either String VKStructs.VKResult ->
  IO Config.Config
checkResult _ _ _ (Left err) = BotEx.throwSendExcept (Logger.makeLogMessage LoggerMsgs.sndMsgFld (T.pack err))
checkResult _ _ _ (Right (VKStructs.SendMsgError (VKStructs.SendError err))) =
  BotEx.throwSendExcept (Logger.makeLogMessage LoggerMsgs.sndMsgFld (VKStructs.errMsg err))
checkResult config logger msg (Right (VKStructs.SendMsgScs _)) = do
  liftIO $ Logger.botLog logger LoggerMsgs.sndMsgScsVK
  pure (Config.configSetOffset config (PureStructs.updateID msg))
