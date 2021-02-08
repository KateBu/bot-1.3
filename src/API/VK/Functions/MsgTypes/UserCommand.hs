module API.VK.Functions.MsgTypes.UserCommand where

import qualified API.VK.Structs.Exports as VKStructs
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

buildCommandMessage :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
buildCommandMessage vkMsg = VKStructs.msg_text vkMsg >>= buildCommandMessage'

buildCommandMessage' :: T.Text -> Maybe PureStructs.MessageType
buildCommandMessage' "/help" = pure $ PureStructs.MsgTypeUserCommand PureStructs.Help
buildCommandMessage' "/repeat" = pure $ PureStructs.MsgTypeUserCommand PureStructs.Repeat
buildCommandMessage' _ = Nothing
