module Logic.PureStructs where

import API.Telegram.Structs
import Config.Config
import Logger.Logger

import qualified Data.Text as T 


type UpdateID = Integer 
type MbCaption = Maybe T.Text


data Message = EmptyMessage UpdateID
    | UserCommand UpdateID Command
    | CommonMessage UpdateID Integer CMessage MbCaption
    | CallbackQuery UpdateID Integer T.Text

data Command = Command 
    {
         chatID :: Integer
        , text :: T.Text
    }

data CMessage = Txt  T.Text 
    | Animation  TelAmination 
    | Audio  TelAudio 
    | Document  TelDocument 
    | Photo  [TelPhoto] 
    | Video  TelVideo 
    | Voice  TelVoice 
    | Contact  TelContact 
    | Poll  TelPoll 
    | Venue  TelVenue 
    | Location  TelLocation 
    | Sticker  TelSticker 
    | Buttons [[Button]]
    | Other 


data ProcessMessageResult = ProcessMessageResult 
    {
        updID :: UpdateID
        , msgType :: T.Text
        , newConfig :: Config
        , mbChatID :: Maybe Integer 
        , mbMessage :: Maybe CMessage
        , mbCaption :: MbCaption
    }




getMsgType :: Message -> T.Text
getMsgType (EmptyMessage _) = "EmptyMessage"
getMsgType (UserCommand _ _) = "UserCommand"
getMsgType (CommonMessage _ _ _ _) = "CommonMessage"
getMsgType (CallbackQuery _ _ _) = "CallbackQuery"


getUid :: Message -> UpdateID
getUid (EmptyMessage uid) = uid 
getUid (UserCommand uid _) = uid 
getUid (CommonMessage uid _ _ _) = uid 

getContentType :: Message -> T.Text
getContentType (CommonMessage _ _ cMsg _) = getMessageType cMsg 


getMessageType :: CMessage -> T.Text
getMessageType (Txt _) = "Message"
getMessageType (Animation _)  = "Animation"
getMessageType (Audio _) = "Audio"
getMessageType (Document _) = "Document"
getMessageType (Photo _) = "Photo"
getMessageType (Video _) = "Video"
getMessageType (Voice _) = "Voice"
getMessageType (Contact _)  = "Contact"
getMessageType (Poll _)  = "Poll"
getMessageType (Venue _)  = "Venue"
getMessageType (Location _)  = "Location"
getMessageType (Sticker _) = "Sticker"
getMessageType (Buttons _) = "Message"


buttons :: [[Button]]
buttons = [[Button "1" "/setRepetition1"]
    , [Button "2" "/setRepetition2"]
    , [Button "3" "/setRepetition3"]
    , [Button "4" "/setRepetition4"]
    , [Button "5" "/setRepetition5"]]

repeatText :: T.Text
repeatText = "Выберите количество повторов:"

newRepeatText :: Int -> T.Text
newRepeatText rep = "Установлено количество обновлений: " <> (T.pack . show) rep



