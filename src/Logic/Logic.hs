module Logic.Logic where

import qualified Data.Text as T
import qualified Environment.Environment as Env
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs

type SendFunction m =
  (Env.Env m -> PureStructs.PureMessage -> m (Env.Env m))

processMsgs ::
  (Monad m) =>
  Env.Env m ->
  SendFunction m ->
  [PureStructs.PureMessage] ->
  m (Env.Env m)
processMsgs env sendFunction msgs =
  mapM (processMsgs_ env sendFunction) msgs >>= getLast env

processMsgs_ ::
  (Monad m) =>
  Env.Env m ->
  SendFunction m ->
  PureStructs.PureMessage ->
  m (Env.Env m)
processMsgs_ env sendFunction msg = case PureStructs.messageType msg of
  PureStructs.MTEmpty -> Env.eSetOffset ((succ . PureStructs.updateID) msg) env
  PureStructs.MTUserCommand PureStructs.Help -> sendFunction env msg
  PureStructs.MTUserCommand PureStructs.Repeat -> sendFunction env (makeRepeatMsg msg)
  PureStructs.MTCallbackQuery callbackData -> do
    let mbChid = PureStructs.mbChatID msg
    maybe processMsgsErr (processMsgsCallback env sendFunction msg callbackData) mbChid
  PureStructs.MTCommon _ -> do
    let mbChid = PureStructs.mbChatID msg
    maybe processMsgsErr (processMsgsCommon env sendFunction msg) mbChid

processMsgsErr :: Monad m => m (Env.Env m)
processMsgsErr = BotEx.throwOtherException LoggerMsgs.chidNotFound

processMsgsCallback ::
  Monad m =>
  Env.Env m ->
  SendFunction m ->
  PureStructs.PureMessage ->
  T.Text ->
  PureStructs.ChatID ->
  m (Env.Env m)
processMsgsCallback env function msg callbackData chid = do
  let newRep = PureStructs.getNewRep callbackData
  newEnv <- Env.eSetUserRepeat chid newRep env
  function newEnv (makeCallbackResponse msg)

processMsgsCommon ::
  Monad m =>
  Env.Env m ->
  SendFunction m ->
  PureStructs.PureMessage ->
  PureStructs.ChatID ->
  m (Env.Env m)
processMsgsCommon env function msg chid = do
  newRepeat <- Env.eFindUserRepeat chid env
  repeatMsg msg newRepeat function env

repeatMsg ::
  Monad m =>
  PureStructs.PureMessage ->
  Int ->
  SendFunction m ->
  Env.Env m ->
  m (Env.Env m)
repeatMsg _ 0 _ env = pure env
repeatMsg msg n function env = do
  function env msg
    >>= repeatMsg msg (n -1) function

getLast :: Monad m => Env.Env m -> [Env.Env m] -> m (Env.Env m)
getLast env [] = pure env
getLast _ envs = pure . last $ envs

makeRepeatMsg :: PureStructs.PureMessage -> PureStructs.PureMessage
makeRepeatMsg msg =
  PureStructs.PureMessage
    (PureStructs.MTCommon "Message")
    (PureStructs.updateID msg)
    (PureStructs.mbChatID msg)
    (mconcat [PureStructs.mbParams msg, Just [PureStructs.ParamsText "text" PureStructs.repeatText]])

makeCallbackResponse :: PureStructs.PureMessage -> PureStructs.PureMessage
makeCallbackResponse msg = msg {PureStructs.messageType = PureStructs.MTCommon "Message"}
