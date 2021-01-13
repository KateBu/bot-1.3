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
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs
import Network.HTTP.Req
  ( LbsResponse,
    responseBody,
    responseStatusCode,
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
