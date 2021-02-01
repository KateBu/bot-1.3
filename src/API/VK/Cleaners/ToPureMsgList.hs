module API.VK.Cleaners.ToPureMsgList (vkByteStringToPureMessageList) where

import API.VK.Cleaners.ToPureMessages (mkPureMessage)
import qualified API.VK.Structs.Internals as VKStructs
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Internals as Env
import qualified Environment.Logger.Internals as Logger
import qualified Environment.Logger.Messages as LoggerMsgs
import qualified Exceptions.Internals as BotEx
import qualified Logic.PureStructs as PureStructs
import Text.Read (readMaybe)

vkByteStringToPureMessageList ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
vkByteStringToPureMessageList env bs =
  decByteString env bs
    >>= vkUpdInfoToPureMessageList env

decByteString ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO (PureStructs.UpdateID, [VKStructs.VKUpdInfo])
decByteString env bs = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.vkDecBS
  let eiUpdates = eitherDecode bs :: Either String VKStructs.VKUpdates
  either decodeUpdErr (decodeUpdScs logger) eiUpdates

decodeUpdErr ::
  String ->
  IO (PureStructs.UpdateID, [VKStructs.VKUpdInfo])
decodeUpdErr err =
  BotEx.throwUpdateExcept (Logger.makeLogMessage LoggerMsgs.vkDecUpdatesFailed $ T.pack err)

decodeUpdScs ::
  Logger.Logger IO ->
  VKStructs.VKUpdates ->
  IO (PureStructs.UpdateID, [VKStructs.VKUpdInfo])
decodeUpdScs _ (VKStructs.VKUpdateError (VKStructs.UpdateErr errCode _)) = case errCode of
  1 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailed1
  2 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailed2
  3 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailed3
  _ -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailed4
decodeUpdScs logger (VKStructs.VKUpdates upd) = do
  Logger.botLog logger LoggerMsgs.vkDecBsScs
  let mbOffset = readMaybe $ VKStructs.ts upd :: Maybe Int
  maybe
    (BotEx.throwOtherException LoggerMsgs.readValueFld)
    (\offset -> pure (offset, VKStructs.updates upd))
    mbOffset

vkUpdInfoToPureMessageList ::
  Env.Environment IO ->
  (PureStructs.UpdateID, [VKStructs.VKUpdInfo]) ->
  IO [PureStructs.PureMessage]
vkUpdInfoToPureMessageList env (uid, upds) = do
  logger <- runReaderT Env.eLogger env
  hMsg <- runReaderT Env.eHelpMsg env
  Logger.botLog logger LoggerMsgs.parseVKMsgScs
  pure $ vkUpdInfoToPureMessage hMsg uid <$> upds

vkUpdInfoToPureMessage ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  VKStructs.VKUpdInfo ->
  PureStructs.PureMessage
vkUpdInfoToPureMessage hMsg uid updInfo = case VKStructs.updType updInfo of
  VKStructs.OtherEvent -> BotEx.throwPureOtherException LoggerMsgs.unexpVKEvent
  _ -> do
    let mbUpdObj = VKStructs.updObj updInfo
    maybe (noUpdObj uid) (justUpdObj hMsg uid) mbUpdObj

noUpdObj :: PureStructs.UpdateID -> PureStructs.PureMessage
noUpdObj uid = PureStructs.PureMessage PureStructs.MTEmpty uid Nothing Nothing

justUpdObj ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  VKStructs.VKObject ->
  PureStructs.PureMessage
justUpdObj hMsg uid obj = do
  let vkMessage = VKStructs.vkMessage obj
  mkPureMessage hMsg uid vkMessage
