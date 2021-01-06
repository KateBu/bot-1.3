module API.Wrapper where

import qualified API.Telegram.Cleaners as TelCleaners
import qualified API.VK.Cleaners.ToPureMsgList as VKCleaners
import qualified API.VK.Structs as VKStructs
import qualified API.WrapFunctions as WrapFunctions
import qualified API.WrapStructs as WrapStructs
import qualified Config.Config as Config
import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
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
    responseBody,
    responseStatusCode,
    runReq,
  )

getPureMessageList :: Config.Config -> Logger.Logger -> IO (Either BotEx.BotException [PureStructs.PureMessage])
getPureMessageList config logger = getU config >>= byteStringToPureMessageList config logger

getU :: Config.Config -> IO (Either BotEx.BotException BSL.ByteString)
getU config = do
  let params = (WrapFunctions.updateParam (Config.botType config))
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
  IO (Either BotEx.BotException LbsResponse)
getResponseMultipart Nothing _ _ = pure $ BotEx.throwOtherException LoggerMsgs.invalidHP
getResponseMultipart (Just url) params options = do
  multipartParams <- WrapFunctions.paramsToMultipartBody params
  catch
    ( runReq defaultHttpConfig $ do
        response <-
          req
            POST
            url
            multipartParams
            lbsResponse
            options
        (pure . pure) response
    )
    (\ex -> BotEx.throwIOException (ex :: IOException))

getResponseUrl ::
  Maybe (Url 'Https) ->
  [PureStructs.Params] ->
  Option 'Https ->
  IO (Either BotEx.BotException LbsResponse)
getResponseUrl Nothing _ _ = pure $ BotEx.throwOtherException LoggerMsgs.invalidHP
getResponseUrl (Just ulr) params options =
  catch
    ( runReq defaultHttpConfig $ do
        response <-
          req
            POST
            ulr
            (WrapFunctions.paramsToUrlBody params)
            lbsResponse
            options
        (pure . pure) response
    )
    (\ex -> BotEx.throwIOException (ex :: IOException))

responseToLbsByteString ::
  Either BotEx.BotException LbsResponse ->
  IO (Either BotEx.BotException BSL.ByteString)
responseToLbsByteString (Left err) = pure $ Left err
responseToLbsByteString (Right response) = case responseStatusCode response of
  200 -> pure $ Right (responseBody response :: BSL.ByteString)
  err ->
    pure $
      BotEx.throwOtherException
        (Logger.makeLogMessage LoggerMsgs.badServerResponse ((T.pack . show) err))

byteStringToPureMessageList ::
  Config.Config ->
  Logger.Logger ->
  Either BotEx.BotException BSL.ByteString ->
  IO (Either BotEx.BotException [PureStructs.PureMessage])
byteStringToPureMessageList config@(Config.Config (Config.VKBot _) _ _ _ _) logger eiBS =
  VKCleaners.vkByteStringToPureMessageList config logger eiBS
byteStringToPureMessageList config@(Config.Config (Config.TBot _) _ _ _ _) logger eiBS =
  TelCleaners.telByteStringToPureMessageList config logger eiBS

sendM ::
  Config.Config ->
  Logger.Logger ->
  PureStructs.PureMessage ->
  IO (Either BotEx.BotException Config.Config)
sendM config logger msg = do
  let mbParams = PureStructs.mbParams msg
  maybe (pure . Right $ config) (mkSendConfig config logger msg) mbParams

mkSendConfig ::
  Config.Config ->
  Logger.Logger ->
  PureStructs.PureMessage ->
  [PureStructs.Params] ->
  IO (Either BotEx.BotException Config.Config)
mkSendConfig config logger msg params = do
  basicParams <- WrapFunctions.mbSendOption (Config.botType config)
  getApiResponse config basicParams params msg >>= checkApiResponse config logger msg

getApiResponse ::
  Config.Config ->
  Option 'Https ->
  [PureStructs.Params] ->
  PureStructs.PureMessage ->
  IO (Either BotEx.BotException LbsResponse)
getApiResponse config basicParams params msg = do
  let hostPath = WrapFunctions.mkHostPath config WrapStructs.Send (Just msg)
  if any WrapFunctions.isMultipart params
    then getResponseMultipart hostPath params basicParams
    else getResponseUrl hostPath params basicParams

checkApiResponse ::
  Config.Config ->
  Logger.Logger ->
  PureStructs.PureMessage ->
  Either BotEx.BotException LbsResponse ->
  IO (Either BotEx.BotException Config.Config)
checkApiResponse _ _ _ (Left err) = pure $ Left err
checkApiResponse config logger msg (Right lbsResp) = case responseStatusCode lbsResp of
  200 -> newBotConfig config (Config.botType config) logger msg lbsResp
  err -> pure . BotEx.throwSendExcept $ Logger.makeLogMessage LoggerMsgs.sndMsgFld ((T.pack . show) err)

newBotConfig ::
  Config.Config ->
  Config.BotType ->
  Logger.Logger ->
  PureStructs.PureMessage ->
  LbsResponse ->
  IO (Either BotEx.BotException Config.Config)
newBotConfig config (Config.VKBot _) logger msg lbsResp = do
  let sndMsgResult = eitherDecode (responseBody lbsResp) :: Either String VKStructs.VKResult
  checkResult config logger msg sndMsgResult
newBotConfig config (Config.TBot _) logger msg _ = do
  liftIO $ Logger.botLog logger LoggerMsgs.sndMsgScsTel
  pure $ Right (Config.configSetOffset config (succ (PureStructs.updateID msg)))

checkResult ::
  Config.Config ->
  Logger.Logger ->
  PureStructs.PureMessage ->
  Either String VKStructs.VKResult ->
  IO (Either BotEx.BotException Config.Config)
checkResult _ _ _ (Left err) = pure $ BotEx.throwSendExcept (Logger.makeLogMessage LoggerMsgs.sndMsgFld (T.pack err))
checkResult _ _ _ (Right (VKStructs.SendMsgError (VKStructs.SendError err))) =
  pure $
    BotEx.throwSendExcept (Logger.makeLogMessage LoggerMsgs.sndMsgFld (VKStructs.errMsg err))
checkResult config logger msg (Right (VKStructs.SendMsgScs _)) = do
  liftIO $ Logger.botLog logger LoggerMsgs.sndMsgScsVK
  pure $ pure (Config.configSetOffset config (PureStructs.updateID msg))
