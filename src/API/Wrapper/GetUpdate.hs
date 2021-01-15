module API.Wrapper.GetUpdate where

import qualified API.Telegram.Cleaners as TelCleaners
import qualified API.VK.Cleaners.ToPureMsgList as VKCleaners
import API.Wrapper.GetResponseFunctions
  ( getResponseMultipart,
    getResponseUrl,
  )
import qualified API.Wrapper.WrapFunctions as WrapFunctions
import qualified API.Wrapper.WrapStructs as WrapStructs
import qualified Config.Config as Config
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Environment as Env
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs
import Network.HTTP.Req
  ( LbsResponse,
    responseBody,
    responseStatusCode,
  )

getPureMessageList :: Env.Env IO -> IO [PureStructs.PureMessage]
getPureMessageList env = getU env >>= byteStringToPureMessageList env

getU :: Env.Env IO -> IO BSL.ByteString
getU env = do
  currentConfig <- Env.eGetConfig env
  botType <- Env.eGetBotType env
  let params = WrapFunctions.updateParam botType
  let url = WrapFunctions.mkHostPath currentConfig WrapStructs.Update Nothing
  lbsResp <-
    if any WrapFunctions.isMultipart params
      then do
        Env.eLog LoggerMsgs.getRespMulti env
        getResponseMultipart url params mempty
      else do
        Env.eLog LoggerMsgs.getRespUrl env
        getResponseUrl url params mempty
  responseToLbsByteString lbsResp

byteStringToPureMessageList ::
  Env.Env IO ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
byteStringToPureMessageList env eiBS = do
  currentConfig <- Env.eGetConfig env
  case currentConfig of
    (Config.Config (Config.VKBot _) _ _ _ _) ->
      VKCleaners.vkByteStringToPureMessageList env eiBS
    (Config.Config (Config.TBot _) _ _ _ _) ->
      TelCleaners.telByteStringToPureMessageList env eiBS

responseToLbsByteString ::
  LbsResponse ->
  IO BSL.ByteString
responseToLbsByteString response = case responseStatusCode response of
  200 -> pure (responseBody response :: BSL.ByteString)
  err ->
    BotEx.throwOtherException
      (Logger.makeLogMessage LoggerMsgs.badServerResponse ((T.pack . show) err))
