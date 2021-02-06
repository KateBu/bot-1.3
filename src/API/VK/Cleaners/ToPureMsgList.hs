module API.VK.Cleaners.ToPureMsgList (vkByteStringToPureMessageList) where

import API.VK.Cleaners.ToPureMessages (makePureMessage)
import qualified API.VK.Structs.Exports as VKStructs
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Environment.Logger.Exports as Logger
import qualified Exceptions.Exports as BotEx
import qualified Logic.PureStructs as PureStructs
import Text.Read (readMaybe)
import qualified TextMessages.LoggerMessages as LoggerMsgs

vkByteStringToPureMessageList ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
vkByteStringToPureMessageList env vkByteString = do
  decodedUpdateIdUpdateInfo <- decodeByteString env vkByteString
  vkUpdateInfoToPureMessageList env decodedUpdateIdUpdateInfo

decodeByteString ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO (PureStructs.UpdateID, [VKStructs.VKUpdInfo])
decodeByteString env vkByteString = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.vkByteStringDecodingInProgress
  let eiVKUpdates = eitherDecode vkByteString :: Either String VKStructs.VKUpdates
  either decodeUpdateError (decodeUpdateSuccess logger) eiVKUpdates

decodeUpdateError ::
  String ->
  IO (PureStructs.UpdateID, [VKStructs.VKUpdInfo])
decodeUpdateError err =
  BotEx.throwUpdateExcept (Logger.makeLogMessage LoggerMsgs.vkUpdatesDecodingFailed $ T.pack err)

decodeUpdateSuccess ::
  Logger.Logger IO ->
  VKStructs.VKUpdates ->
  IO (PureStructs.UpdateID, [VKStructs.VKUpdInfo])
decodeUpdateSuccess _ (VKStructs.VKUpdateError (VKStructs.UpdateErr errCode _)) = case errCode of
  1 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailedCode1
  2 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailedCode2
  3 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailedCode3
  _ -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailedCode4
decodeUpdateSuccess logger (VKStructs.VKUpdates updates) = do
  Logger.botLog logger logMsg
  let mbUpdateId = readMaybe $ VKStructs.ts updates :: Maybe Int
  maybe
    (BotEx.throwOtherException LoggerMsgs.readUpdateIdFailed)
    (\updateId -> pure (updateId, VKStructs.updates updates))
    mbUpdateId
  where
    logMsg = LoggerMsgs.vkDecodeByteStringSuccess

vkUpdateInfoToPureMessageList ::
  Env.Environment IO ->
  (PureStructs.UpdateID, [VKStructs.VKUpdInfo]) ->
  IO [PureStructs.PureMessage]
vkUpdateInfoToPureMessageList env (updateId, updates) = do
  logger <- runReaderT Env.eLogger env
  helpMsg <- runReaderT Env.eHelpMsg env
  Logger.botLog logger LoggerMsgs.parseVKMsgSuccess
  maybe (BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailed) pure (mbPureMsgs helpMsg)
  where
    mbPureMsgs helpMsg = sequence $ vkUpdInfoToPureMessage helpMsg updateId <$> updates

vkUpdInfoToPureMessage ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  VKStructs.VKUpdInfo ->
  Maybe PureStructs.PureMessage
vkUpdInfoToPureMessage helpMsg updateId updateInfo = case VKStructs.update_type updateInfo of
  VKStructs.OtherEvent -> Nothing
  _ -> do
    let mbUpdateObject = VKStructs.update_object updateInfo
    maybe (makeEmptyMsg updateId) (makeMsgWithParams helpMsg updateId) mbUpdateObject

makeEmptyMsg :: PureStructs.UpdateID -> Maybe PureStructs.PureMessage
makeEmptyMsg updateId = pure $ PureStructs.PureMessage PureStructs.MsgTypeEmpty updateId Nothing Nothing

makeMsgWithParams ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  VKStructs.VKObject ->
  Maybe PureStructs.PureMessage
makeMsgWithParams helpMsg updateId object = do
  let vkMessage = VKStructs.vkMessage object
  makePureMessage helpMsg updateId vkMessage
