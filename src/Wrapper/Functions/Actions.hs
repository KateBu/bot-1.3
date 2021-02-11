module Wrapper.Functions.Actions (updateEnvironment) where

import qualified API.PureStructs.Exports as PureStructs
import qualified API.VK.Structs.Exports as VKStructs
import qualified Config.Exports as Config
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (eitherDecode)
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Exceptions.Exports as BotEx
import qualified Logger.Exports as Logger
import Network.HTTP.Req
  ( LbsResponse,
    responseBody,
    responseStatusCode,
  )
import qualified TextMessages.LoggerMessages as LoggerMsgs

updateEnvironment ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO (Env.Environment IO)
updateEnvironment env msg apiResponse =
  case responseStatusCode apiResponse of
    200 -> updateEnvironment' env msg apiResponse
    err -> BotEx.throwSendExcept $ Logger.makeLogMessage LoggerMsgs.sendMsgFailed ((T.pack . show) err)

updateEnvironment' ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO (Env.Environment IO)
updateEnvironment' env msg lbsResp = do
  logger <- runReaderT Env.eLogger env
  config <- runReaderT Env.eConfig env
  case config of
    Config.VKBot _ -> do
      let sendMsgResult = eitherDecode (responseBody lbsResp) :: Either String VKStructs.Result
      vKUpdateResult env msg sendMsgResult
    Config.TBot _ -> do
      Logger.botLog logger LoggerMsgs.sendTelegramMsgSuccess
      Env.eSetOffset env $ succ (PureStructs.updateID msg)

vKUpdateResult ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  Either String VKStructs.Result ->
  IO (Env.Environment IO)
vKUpdateResult _ _ (Left err) = BotEx.throwSendExcept logMsg
  where
    logMsg = Logger.makeLogMessage LoggerMsgs.sendMsgFailed (T.pack err)
vKUpdateResult _ _ (Right (VKStructs.SendMsgError (VKStructs.SendError err))) =
  BotEx.throwSendExcept logMsg
  where
    logMsg = Logger.makeLogMessage LoggerMsgs.sendMsgFailed (VKStructs.err_msg err)
vKUpdateResult env msg (Right (VKStructs.SendMsgSuccess _)) = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.sendVkMsgSuccess
  Env.eSetOffset env $ PureStructs.updateID msg
