module Wrapper.Functions.Actions (updateEnvironment) where

import qualified API.VK.Structs.Exports as VKStructs
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (eitherDecode)
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Exceptions.Exports as BotEx
import qualified Logic.Structs as PureStructs
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
    err -> BotEx.throwSendExcept $ Env.makeLogMessage LoggerMsgs.sendMsgFailed ((T.pack . show) err)

updateEnvironment' ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO (Env.Environment IO)
updateEnvironment' env msg lbsResp = do
  logger <- runReaderT Env.eLogger env
  config <- runReaderT Env.eConfig env
  case config of
    Env.VKBot _ -> do
      let sendMsgResult = eitherDecode (responseBody lbsResp) :: Either String VKStructs.Result
      vKUpdateResult env msg sendMsgResult
    Env.TBot _ -> do
      Env.botLog logger LoggerMsgs.sendTelegramMsgSuccess
      Env.eSetOffset env $ succ (PureStructs.updateID msg)

vKUpdateResult ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  Either String VKStructs.Result ->
  IO (Env.Environment IO)
vKUpdateResult _ _ (Left err) = BotEx.throwSendExcept logMsg
  where
    logMsg = Env.makeLogMessage LoggerMsgs.sendMsgFailed (T.pack err)
vKUpdateResult _ _ (Right (VKStructs.SendMsgError (VKStructs.SendError err))) =
  BotEx.throwSendExcept logMsg
  where
    logMsg = Env.makeLogMessage LoggerMsgs.sendMsgFailed (VKStructs.err_msg err)
vKUpdateResult env msg (Right (VKStructs.SendMsgSuccess _)) = do
  logger <- runReaderT Env.eLogger env
  Env.botLog logger LoggerMsgs.sendVkMsgSuccess
  Env.eSetOffset env $ PureStructs.updateID msg
