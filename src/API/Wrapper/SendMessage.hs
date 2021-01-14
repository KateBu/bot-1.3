module API.Wrapper.SendMessage where

import qualified API.VK.Structs as VKStructs
import API.Wrapper.GetResponseFunctions
  ( getResponseMultipart,
    getResponseUrl,
  )
import qualified API.Wrapper.WrapFunctions as WrapFunctions
import qualified API.Wrapper.WrapStructs as WrapStructs
import qualified Config.Config as Config
--import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (eitherDecode)
import qualified Data.Text as T
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs
import qualified Environment.Environment as Env 
import Control.Monad.Reader 
import Network.HTTP.Req
  ( LbsResponse,
    Option,
    Scheme (Https),
    responseBody,
    responseStatusCode,
  )

sendM' ::
  Env.Env IO ->
  PureStructs.PureMessage ->
  IO (Env.Env IO)
sendM' env msg = do
  let mbParams = PureStructs.mbParams msg
  maybe (pure env) (mkSendConfig' env msg) mbParams

mkSendConfig' ::
  Env.Env IO ->
  PureStructs.PureMessage ->
  [PureStructs.Params] ->
  IO (Env.Env IO)
mkSendConfig' env msg params = do
  config <- runReaderT Env.eGetConfig env
  basicParams <- WrapFunctions.mbSendOption (Config.botType config) 
  getApiResponse' env basicParams params msg >>= checkApiResponse' env msg

getApiResponse' ::
  Env.Env IO ->
  Option 'Https ->
  [PureStructs.Params] ->
  PureStructs.PureMessage ->
  IO LbsResponse
getApiResponse' env basicParams params msg = do
  config <- runReaderT Env.eGetConfig env 
  let hostPath = WrapFunctions.mkHostPath config WrapStructs.Send (Just msg)
  if any WrapFunctions.isMultipart params
    then getResponseMultipart hostPath params basicParams
    else getResponseUrl hostPath params basicParams

checkApiResponse' ::
  Env.Env IO ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO (Env.Env IO)
checkApiResponse' env msg lbsResp = case responseStatusCode lbsResp of
  200 -> newBotConfig' env msg lbsResp
  err -> BotEx.throwSendExcept $ Logger.makeLogMessage LoggerMsgs.sndMsgFld ((T.pack . show) err)

newBotConfig' ::
  Env.Env IO ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO (Env.Env IO)
newBotConfig' env msg lbsResp = do
  config <- runReaderT Env.eGetConfig env 
  case Config.botType config of 
    Config.VKBot _ -> do
      let sndMsgResult = eitherDecode (responseBody lbsResp) :: Either String VKStructs.VKResult
      checkResult' env msg sndMsgResult
    Config.TBot _ -> do
      runReaderT (Env.eLog LoggerMsgs.sndMsgScsTel) env 
      runReaderT (Env.eSetOffset (succ (PureStructs.updateID msg))) env 


checkResult' ::
  Env.Env IO ->
  PureStructs.PureMessage ->
  Either String VKStructs.VKResult ->
  IO (Env.Env IO )
checkResult' _ _ (Left err) = BotEx.throwSendExcept (Logger.makeLogMessage LoggerMsgs.sndMsgFld (T.pack err))
checkResult' _ _ (Right (VKStructs.SendMsgError (VKStructs.SendError err))) =
  BotEx.throwSendExcept (Logger.makeLogMessage LoggerMsgs.sndMsgFld (VKStructs.errMsg err))
checkResult' env msg (Right (VKStructs.SendMsgScs _)) = do
  runReaderT (Env.eLog LoggerMsgs.sndMsgScsVK) env 
  runReaderT (Env.eSetOffset (PureStructs.updateID msg)) env 



























sendM ::
  Config.Config ->
  Logger.Logger IO ->
  PureStructs.PureMessage ->
  IO Config.Config
sendM config logger msg = do
  let mbParams = PureStructs.mbParams msg
  maybe (pure config) (mkSendConfig config logger msg) mbParams

mkSendConfig ::
  Config.Config ->
  Logger.Logger IO ->
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
  Logger.Logger IO ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO Config.Config
checkApiResponse config logger msg lbsResp = case responseStatusCode lbsResp of
  200 -> newBotConfig config (Config.botType config) logger msg lbsResp
  err -> BotEx.throwSendExcept $ Logger.makeLogMessage LoggerMsgs.sndMsgFld ((T.pack . show) err)

newBotConfig ::
  Config.Config ->
  Config.BotType ->
  Logger.Logger IO ->
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
  Logger.Logger IO ->
  PureStructs.PureMessage ->
  Either String VKStructs.VKResult ->
  IO Config.Config
checkResult _ _ _ (Left err) = BotEx.throwSendExcept (Logger.makeLogMessage LoggerMsgs.sndMsgFld (T.pack err))
checkResult _ _ _ (Right (VKStructs.SendMsgError (VKStructs.SendError err))) =
  BotEx.throwSendExcept (Logger.makeLogMessage LoggerMsgs.sndMsgFld (VKStructs.errMsg err))
checkResult config logger msg (Right (VKStructs.SendMsgScs _)) = do
  liftIO $ Logger.botLog logger LoggerMsgs.sndMsgScsVK
  pure (Config.configSetOffset config (PureStructs.updateID msg))
