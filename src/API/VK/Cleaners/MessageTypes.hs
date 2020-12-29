module API.VK.Cleaners.MessageTypes where

import qualified Data.Text as T 
import Control.Applicative ( Alternative((<|>)) ) 
import qualified API.VK.Structs as VKStructs 
import qualified Logic.PureStructs as PureStructs
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs

getMessageType :: VKStructs.VKMessage -> Either Logger.LogMessage PureStructs.MessageType
getMessageType vkMsg = case msgType of 
    Nothing -> Left LoggerMsgs.notImplemented 
    Just mType -> Right mType 
    where msgType = mbCallBackMsg vkMsg
            <|> mbUserCommand vkMsg
            <|> mbAttachmentMsg vkMsg
            <|> mbGeo vkMsg
            <|> mbFwd vkMsg 
            <|> mbTextMsg vkMsg              
    
mbCallBackMsg, mbUserCommand
    , mbAttachmentMsg, mbTextMsg
    , mbGeo, mbFwd :: VKStructs.VKMessage -> Maybe PureStructs.MessageType 

mbCallBackMsg vkMsg = case VKStructs.cbPayload vkMsg of 
    Nothing -> Nothing 
    Just callback -> 
        let result = mbRep1 callback
                <|> mbRep2 callback 
                <|> mbRep3 callback
                <|> mbRep4 callback 
                <|> mbRep5 callback
        in case result of 
                Nothing -> Nothing 
                Just val -> Just $ PureStructs.MTCallbackQuery val 

mbUserCommand vkMsg = case VKStructs.msgText vkMsg of  
    Nothing -> Nothing 
    Just "" -> Nothing 
    Just txt -> case txt of 
        "/help" -> pure $ PureStructs.MTUserCommand PureStructs.Help
        "/repeat"  -> pure $ PureStructs.MTUserCommand PureStructs.Repeat
        _ -> Nothing 

mbAttachmentMsg vkMsg = case VKStructs.attachments vkMsg of 
    Nothing -> Nothing 
    Just [] -> Nothing 
    _ -> pure $ PureStructs.MTCommon "Attachment"

mbTextMsg vkMsg = case VKStructs.msgText vkMsg of 
    Nothing -> Nothing 
    Just "" -> Nothing 
    _ -> pure $ PureStructs.MTCommon "Message"

mbGeo vkMsg = case VKStructs.geo vkMsg of 
    Nothing -> Nothing 
    _ -> pure $ PureStructs.MTCommon "Geo"

mbFwd vkMsg = case VKStructs.fwdMessages vkMsg of 
    Nothing -> Nothing 
    _ -> pure $ PureStructs.MTCommon "Fwd"

mbRep1 :: T.Text -> Maybe T.Text 
mbRep1 txt = if T.isInfixOf PureStructs.rep1 txt 
    then pure PureStructs.rep1 
    else Nothing 

mbRep2 :: T.Text -> Maybe T.Text 
mbRep2 txt = if T.isInfixOf PureStructs.rep2 txt 
    then pure PureStructs.rep2 
    else Nothing 

mbRep3 :: T.Text -> Maybe T.Text 
mbRep3 txt = if T.isInfixOf PureStructs.rep3 txt 
    then pure PureStructs.rep3 
    else Nothing 

mbRep4 :: T.Text -> Maybe T.Text 
mbRep4 txt = if T.isInfixOf PureStructs.rep4 txt 
    then pure PureStructs.rep4 
    else Nothing 

mbRep5 :: T.Text -> Maybe T.Text 
mbRep5 txt = if T.isInfixOf PureStructs.rep5 txt 
    then pure PureStructs.rep5 
    else Nothing 