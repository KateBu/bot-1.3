module API.Wrapper.SendMessage where

import qualified API.VK.Structs as VKStructs
import API.Wrapper.GetResponseFunctions
  ( getResponseMultipart,
    getResponseUrl,
  )
import qualified API.Wrapper.WrapFunctions as WrapFunctions
import qualified API.Wrapper.WrapStructs as WrapStructs
import qualified Config.Config as Config
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (eitherDecode)
import qualified Data.Text as T
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs
import Network.HTTP.Req
  ( LbsResponse,
    Option,
    Scheme (Https),
    responseBody,
    responseStatusCode,
  )

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
