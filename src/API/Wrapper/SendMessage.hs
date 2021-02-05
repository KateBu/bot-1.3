module API.Wrapper.SendMessage where

import qualified API.VK.Structs.Exports as VKStructs
import qualified API.Wrapper.Functions as WrapFunctions
import API.Wrapper.GetResponseFunctions
  ( getResponseMultipart,
    getResponseUrl,
  )
import qualified API.Wrapper.Structs as WrapStructs
import qualified Config.Exports as Config
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (eitherDecode)
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Environment.Logger.Exports as Logger
import qualified Exceptions.Exports as BotEx
import qualified Logic.PureStructs as PureStructs
import Network.HTTP.Req
  ( LbsResponse,
    Option,
    Scheme (Https),
    responseBody,
    responseStatusCode,
  )
import qualified TextMessages.LoggerMessages as LoggerMsgs

sendMessage ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  IO (Env.Environment IO)
sendMessage env msg = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.sendMsgInProgress
  let mbParams = PureStructs.mbParams msg
  maybe (pure env) (sendMessage' env msg) mbParams

sendMessage' ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  [PureStructs.Params] ->
  IO (Env.Environment IO)
sendMessage' env msg params = do
  config <- runReaderT Env.eConfig env
  basicParams <- WrapFunctions.mbSendOption config
  apiResponse <- getApiResponse env basicParams params msg
  checkApiResponse env msg apiResponse

getApiResponse ::
  Env.Environment IO ->
  Option 'Https ->
  [PureStructs.Params] ->
  PureStructs.PureMessage ->
  IO LbsResponse
getApiResponse env basicParams params msg = do
  logger <- runReaderT Env.eLogger env
  config <- runReaderT Env.eConfig env
  Logger.botLog logger LoggerMsgs.getApiResponseInProgress
  let hostPath = WrapFunctions.makeHostPath config WrapStructs.Send (Just msg)
  if any WrapFunctions.isMultipart params
    then getResponseMultipart hostPath params basicParams
    else getResponseUrl hostPath params basicParams

checkApiResponse ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO (Env.Environment IO)
checkApiResponse env msg apiResponse =
  case responseStatusCode apiResponse of
    200 -> updateEnvironment env msg apiResponse
    err -> BotEx.throwSendExcept $ Logger.makeLogMessage LoggerMsgs.sendMsgFailed ((T.pack . show) err)

updateEnvironment ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO (Env.Environment IO)
updateEnvironment env msg lbsResp = do
  logger <- runReaderT Env.eLogger env
  config <- runReaderT Env.eConfig env
  case config of
    Config.VKBot _ -> do
      let sendMsgResult = eitherDecode (responseBody lbsResp) :: Either String VKStructs.VKResult
      checkVKUpdateResult env msg sendMsgResult
    Config.TBot _ -> do
      Logger.botLog logger LoggerMsgs.sendTelegramMsgSuccess
      Env.eSetOffset env $ succ (PureStructs.updateID msg)

checkVKUpdateResult ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  Either String VKStructs.VKResult ->
  IO (Env.Environment IO)
checkVKUpdateResult _ _ (Left err) = BotEx.throwSendExcept logMsg
  where
    logMsg = Logger.makeLogMessage LoggerMsgs.sendMsgFailed (T.pack err)
checkVKUpdateResult _ _ (Right (VKStructs.SendMsgError (VKStructs.SendError err))) =
  BotEx.throwSendExcept logMsg
  where
    logMsg = Logger.makeLogMessage LoggerMsgs.sendMsgFailed (VKStructs.err_msg err)
checkVKUpdateResult env msg (Right (VKStructs.SendMsgSuccess _)) = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.sendVkMsgSuccess
  Env.eSetOffset env $ PureStructs.updateID msg
