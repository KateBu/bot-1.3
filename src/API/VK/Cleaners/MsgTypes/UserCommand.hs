module API.VK.Cleaners.MsgTypes.UserCommand where

import qualified Data.Text as T 
import qualified Logic.PureStructs as PureStructs
import qualified API.VK.Structs as VKStructs 

mbUserCommand :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
mbUserCommand vkMsg = VKStructs.msgText vkMsg >>= mbUserCommand' 

mbUserCommand' :: T.Text -> Maybe PureStructs.MessageType
mbUserCommand' "/help" = pure $ PureStructs.MTUserCommand PureStructs.Help
mbUserCommand' "/repeat" = pure $ PureStructs.MTUserCommand PureStructs.Repeat
mbUserCommand' _ = Nothing