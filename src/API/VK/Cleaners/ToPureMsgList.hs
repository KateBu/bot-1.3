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
  Either BotEx.BotException BSL.ByteString ->
  IO (Either BotEx.BotException [PureStructs.PureMessage])
vkByteStringToPureMessageList config logger bs =
  decByteString logger bs
    >>= vkUpdInfoToPureMessageList config logger

decByteString ::
  Logger.Logger ->
  Either BotEx.BotException BSL.ByteString ->
  IO (Either BotEx.BotException (PureStructs.UpdateID, [VKStructs.VKUpdInfo]))
decByteString logger = either (pure . Left) (decByteString' logger)

decByteString' ::
  Logger.Logger ->
  BSL.ByteString ->
  IO (Either BotEx.BotException (PureStructs.UpdateID, [VKStructs.VKUpdInfo]))
decByteString' logger json = do
  Logger.botLog logger LoggerMsgs.getVKUpdScs
  let eiUpdates = eitherDecode json :: Either String VKStructs.VKUpdates
  either decodeUpdErr decodeUpdScs eiUpdates

decodeUpdScs ::
  VKStructs.VKUpdates ->
  IO (Either BotEx.BotException (PureStructs.UpdateID, [VKStructs.VKUpdInfo]))
decodeUpdScs (VKStructs.VKUpdateError (VKStructs.UpdateErr errCode _)) = case errCode of
  1 -> pure $ BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailed1
  2 -> pure $ BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailed2
  3 -> pure $ BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailed3
  _ -> pure $ BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailed4
decodeUpdScs (VKStructs.VKUpdates upd) =
  pure . pure $
    (read (VKStructs.ts upd), (VKStructs.updates upd))

decodeUpdErr ::
  String ->
  IO (Either BotEx.BotException (PureStructs.UpdateID, [VKStructs.VKUpdInfo]))
decodeUpdErr err =
  pure $
    BotEx.throwUpdateExcept (Logger.makeLogMessage LoggerMsgs.vkDecUpdatesFailed $ T.pack err)

vkUpdInfoToPureMessageList ::
  Config.Config ->
  Logger.Logger ->
  Either BotEx.BotException (PureStructs.UpdateID, [VKStructs.VKUpdInfo]) ->
  IO (Either BotEx.BotException [PureStructs.PureMessage])
vkUpdInfoToPureMessageList _ _ (Left err) = pure $ Left err
vkUpdInfoToPureMessageList config logger (Right (uid, upds)) = do
  Logger.botLog logger LoggerMsgs.parseVKMsgScs
  pure $ mapM (vkUpdInfoToPureMessage config uid) upds

vkUpdInfoToPureMessage ::
  Config.Config ->
  PureStructs.UpdateID ->
  VKStructs.VKUpdInfo ->
  Either BotEx.BotException PureStructs.PureMessage
vkUpdInfoToPureMessage config uid updInfo = case VKStructs.updType updInfo of
  VKStructs.OtherEvent -> BotEx.throwOtherException LoggerMsgs.unexpVKEvent
  _ -> do
    let mbUpdObj = VKStructs.updObj updInfo
    maybe (noUpdObj uid) (justUpdObj config uid) mbUpdObj

justUpdObj ::
  Config.Config ->
  PureStructs.UpdateID ->
  VKStructs.VKObject ->
  Either BotEx.BotException PureStructs.PureMessage
justUpdObj config uid obj = do
  let vkMessage = VKStructs.vkMessage obj
  mkPureMessage config uid vkMessage

noUpdObj :: PureStructs.UpdateID -> Either BotEx.BotException PureStructs.PureMessage
noUpdObj uid = Right (PureStructs.PureMessage PureStructs.MTEmpty uid Nothing Nothing)
