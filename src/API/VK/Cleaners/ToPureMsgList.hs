module API.VK.Cleaners.ToPureMsgList (vkByteStringToPureMessageList) where

import API.VK.Cleaners.ToPureMessages (mkPureMessage)
import qualified API.VK.Structs as VKStructs
import qualified Config.Config as Config
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs

vkByteStringToPureMessageList ::
  Config.Config ->
  Logger.Logger ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
vkByteStringToPureMessageList config logger bs =
  decByteString logger bs
    >>= vkUpdInfoToPureMessageList config logger

decByteString ::
  Logger.Logger ->
  BSL.ByteString ->
  IO (PureStructs.UpdateID, [VKStructs.VKUpdInfo])
decByteString logger json = do
  Logger.botLog logger LoggerMsgs.getVKUpdScs
  let eiUpdates = eitherDecode json :: Either String VKStructs.VKUpdates
  either decodeUpdErr decodeUpdScs eiUpdates

decodeUpdErr ::
  String ->
  IO (PureStructs.UpdateID, [VKStructs.VKUpdInfo])
decodeUpdErr err =
  BotEx.throwUpdateExcept (Logger.makeLogMessage LoggerMsgs.vkDecUpdatesFailed $ T.pack err)

decodeUpdScs ::
  VKStructs.VKUpdates ->
  IO (PureStructs.UpdateID, [VKStructs.VKUpdInfo])
decodeUpdScs (VKStructs.VKUpdateError (VKStructs.UpdateErr errCode _)) = case errCode of
  1 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailed1
  2 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailed2
  3 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailed3
  _ -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailed4
decodeUpdScs (VKStructs.VKUpdates upd) =
  pure $
    (read (VKStructs.ts upd), (VKStructs.updates upd))

vkUpdInfoToPureMessageList ::
  Config.Config ->
  Logger.Logger ->
  (PureStructs.UpdateID, [VKStructs.VKUpdInfo]) ->
  IO [PureStructs.PureMessage]
vkUpdInfoToPureMessageList config logger (uid, upds) = do
  Logger.botLog logger LoggerMsgs.parseVKMsgScs
  pure $ (vkUpdInfoToPureMessage config uid) <$> upds

vkUpdInfoToPureMessage ::
  Config.Config ->
  PureStructs.UpdateID ->
  VKStructs.VKUpdInfo ->
  PureStructs.PureMessage
vkUpdInfoToPureMessage config uid updInfo = case VKStructs.updType updInfo of
  VKStructs.OtherEvent -> BotEx.throwPureOtherException LoggerMsgs.unexpVKEvent
  _ -> do
    let mbUpdObj = VKStructs.updObj updInfo
    maybe (noUpdObj uid) (justUpdObj config uid) mbUpdObj

noUpdObj :: PureStructs.UpdateID -> PureStructs.PureMessage
noUpdObj uid = PureStructs.PureMessage PureStructs.MTEmpty uid Nothing Nothing

justUpdObj ::
  Config.Config ->
  PureStructs.UpdateID ->
  VKStructs.VKObject ->
  PureStructs.PureMessage
justUpdObj config uid obj = do
  let vkMessage = VKStructs.vkMessage obj
  mkPureMessage config uid vkMessage
