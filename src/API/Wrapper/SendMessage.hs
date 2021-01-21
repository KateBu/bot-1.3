module API.Wrapper.SendMessage where

import qualified API.VK.Structs as VKStructs
import API.Wrapper.GetResponseFunctions
  ( getResponseMultipart,
    getResponseUrl,
  )
import qualified API.Wrapper.WrapFunctions as WrapFunctions
import qualified API.Wrapper.WrapStructs as WrapStructs
import qualified Config.Config as Config
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (eitherDecode)
import qualified Data.Text as T
import qualified Environment.Environment as Env
import qualified Environment.Logger.Logger as Logger
import qualified Environment.Logger.LoggerMsgs as LoggerMsgs
import qualified Exceptions.Exceptions as BotEx
import qualified Logic.PureStructs as PureStructs
import Network.HTTP.Req
  ( LbsResponse,
    Option,
    Scheme (Https),
    responseBody,
    responseStatusCode,
  )

sendM ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  IO (Env.Environment IO)
sendM env msg = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.sendMsg
  let mbParams = PureStructs.mbParams msg
  maybe (pure env) (mkSendConfig env msg) mbParams

mkSendConfig ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  [PureStructs.Params] ->
  IO (Env.Environment IO)
mkSendConfig env msg params = do
  config <- runReaderT Env.eConfig env
  basicParams <- WrapFunctions.mbSendOption config
  getApiResponse env basicParams params msg >>= checkApiResponse env msg

getApiResponse ::
  Env.Environment IO ->
  Option 'Https ->
  [PureStructs.Params] ->
  PureStructs.PureMessage ->
  IO LbsResponse
getApiResponse env basicParams params msg = do
  logger <- runReaderT Env.eLogger env
  config <- runReaderT Env.eConfig env
  Logger.botLog logger LoggerMsgs.getApiResp
  let hostPath = WrapFunctions.mkHostPath config WrapStructs.Send (Just msg)
  if any WrapFunctions.isMultipart params
    then getResponseMultipart hostPath params basicParams
    else getResponseUrl hostPath params basicParams

checkApiResponse ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO (Env.Environment IO)
checkApiResponse env msg lbsResp = case responseStatusCode lbsResp of
  200 -> newBotConfig env msg lbsResp
  err -> BotEx.throwSendExcept $ Logger.makeLogMessage LoggerMsgs.sndMsgFld ((T.pack . show) err)

newBotConfig ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO (Env.Environment IO)
newBotConfig env msg lbsResp = do
  logger <- runReaderT Env.eLogger env
  config <- runReaderT Env.eConfig env
  case config of
    Config.VKBot _ -> do
      let sndMsgResult = eitherDecode (responseBody lbsResp) :: Either String VKStructs.VKResult
      checkResult env msg sndMsgResult
    Config.TBot _ -> do
      Logger.botLog logger LoggerMsgs.sndMsgScsTel
      Env.eSetOffset env $ succ (PureStructs.updateID msg)

checkResult ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  Either String VKStructs.VKResult ->
  IO (Env.Environment IO)
checkResult _ _ (Left err) = BotEx.throwSendExcept (Logger.makeLogMessage LoggerMsgs.sndMsgFld (T.pack err))
checkResult _ _ (Right (VKStructs.SendMsgError (VKStructs.SendError err))) =
  BotEx.throwSendExcept (Logger.makeLogMessage LoggerMsgs.sndMsgFld (VKStructs.errMsg err))
checkResult env msg (Right (VKStructs.SendMsgScs _)) = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.sndMsgScsVK
  Env.eSetOffset env $ PureStructs.updateID msg

{-
sendM ::
  Env.Env IO ->
  PureStructs.PureMessage ->
  IO (Env.Env IO)
sendM env msg = do
  let mbParams = PureStructs.mbParams msg
  maybe (pure env) (mkSendConfig env msg) mbParams

mkSendConfig ::
  Env.Env IO ->
  PureStructs.PureMessage ->
  [PureStructs.Params] ->
  IO (Env.Env IO)
mkSendConfig env msg params = do
  botType <- Env.eGetBotType env
  basicParams <- WrapFunctions.mbSendOption botType
  getApiResponse env basicParams params msg >>= checkApiResponse env msg

getApiResponse ::
  Env.Env IO ->
  Option 'Https ->
  [PureStructs.Params] ->
  PureStructs.PureMessage ->
  IO LbsResponse
getApiResponse env basicParams params msg = do
  config <- Env.eGetConfig env
  let hostPath = WrapFunctions.mkHostPath config WrapStructs.Send (Just msg)
  if any WrapFunctions.isMultipart params
    then getResponseMultipart hostPath params basicParams
    else getResponseUrl hostPath params basicParams

checkApiResponse ::
  Env.Env IO ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO (Env.Env IO)
checkApiResponse env msg lbsResp = case responseStatusCode lbsResp of
  200 -> newBotConfig env msg lbsResp
  err -> BotEx.throwSendExcept $ Logger.makeLogMessage LoggerMsgs.sndMsgFld ((T.pack . show) err)

newBotConfig ::
  Env.Env IO ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO (Env.Env IO)
newBotConfig env msg lbsResp = do
  botType <- Env.eGetBotType env
  case botType of
    Config.VKBot _ -> do
      let sndMsgResult = eitherDecode (responseBody lbsResp) :: Either String VKStructs.VKResult
      checkResult env msg sndMsgResult
    Config.TBot _ -> do
      Env.eLog LoggerMsgs.sndMsgScsTel env
      Env.eSetOffset (succ (PureStructs.updateID msg)) env

checkResult ::
  Env.Env IO ->
  PureStructs.PureMessage ->
  Either String VKStructs.VKResult ->
  IO (Env.Env IO)
checkResult _ _ (Left err) = BotEx.throwSendExcept (Logger.makeLogMessage LoggerMsgs.sndMsgFld (T.pack err))
checkResult _ _ (Right (VKStructs.SendMsgError (VKStructs.SendError err))) =
  BotEx.throwSendExcept (Logger.makeLogMessage LoggerMsgs.sndMsgFld (VKStructs.errMsg err))
checkResult env msg (Right (VKStructs.SendMsgScs _)) = do
  Env.eLog LoggerMsgs.sndMsgScsVK env
  Env.eSetOffset (PureStructs.updateID msg) env
-}
