module Logic.Logic where

import qualified Data.Text as T

import Logic.PureStructs
import Logger.Logger  
import Config.Config
import qualified Data.Map as Map 

processMessage :: (Monad m) => Config -> Maybe Message -> m ProcessMessageResult
processMessage _ Nothing  = return Empty
processMessage config (Just (EmptyMessage uid)) = pure $ 
    ProcessMessageResult 
        uid 
        (LogMessage Debug "Empty message processed") 
        (users config) 
        Nothing
        Nothing
        Nothing 
        Nothing
processMessage config (Just (CommonMessage uid chid msg cap)) = let 
        maybeUser = Map.lookup chid (users config) 
    in case maybeUser of 
        Nothing -> pure $
            ProcessMessageResult
                uid 
                (LogMessage Debug "Common Message Processed") 
                (Map.insert chid (repetition config) (users config))
                (Just chid)
                (Just $ repetition config)
                (Just msg) 
                cap
        Just val -> pure $ 
            ProcessMessageResult
                uid 
                (LogMessage Debug "Common Message Processed") 
                (users config)
                (Just chid)
                (Just val)
                (Just msg) 
                cap
processMessage config (Just (UserCommand uid command)) = case text command of 
    "/help" -> pure $ ProcessMessageResult 
        uid 
        (LogMessage Debug "Help Command Processed") 
        (users config)
        (Just $ chatID command)
        (Just 1)
        (Just (Txt (helpMessage config)))
        Nothing 
    "/repeat" -> pure $ ProcessMessageResult 
        uid 
        (LogMessage Debug "Repeat Command recieved, send buttons") 
        (users config)
        (Just $ chatID command)
        (Just 1)
        (Just (Buttons buttons))
        Nothing 
processMessage config (Just (CallbackQuery uid chid query)) = undefined



setRepetitionQuery :: T.Text -> Users -> Users 
setRepetitionQuery query = undefined 








{-
getUpdateId :: UpdateID
        , getLogMsg :: LogMessage
        , getUsers :: Users
        , getChatId :: Maybe Integer
        , getRepetition :: Maybe Int 
        , geMessage :: Maybe CMessage
        , getCaption :: Maybe T.Text
-}