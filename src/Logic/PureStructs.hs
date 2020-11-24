module Logic.PureStructs where

import API.Telegram.Structs
import Config.Config
import Logger.Logger

import qualified Data.Text as T 



--data Updates = TUpdates TelegramUpdates | VKUpdates 

type UpdateID = Integer 
type MbCaption = Maybe T.Text


data Message = EmptyMessage UpdateID
    | UserCommand UpdateID Command
    | CommonMessage UpdateID Integer CMessage MbCaption
    | CallbackQuery UpdateID Integer T.Text

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
getContentType (CommonMessage _ _ (Txt _) _) = "Text"
getContentType (CommonMessage _ _ (Animation _) _) = "Animation"
getContentType (CommonMessage _ _ (Audio _) _) = "Audio"
getContentType (CommonMessage _ _ (Document _) _) = "Document"
getContentType (CommonMessage _ _ (Photo _) _) = "Photo"
getContentType (CommonMessage _ _ (Video _) _) = "Video"
getContentType (CommonMessage _ _ (Voice _) _) = "Voice"
getContentType (CommonMessage _ _ (Contact _) _) = "Contact"
getContentType (CommonMessage _ _ (Poll _) _) = "Poll"
getContentType (CommonMessage _ _ (Venue _) _) = "Venue"
getContentType (CommonMessage _ _ (Location _) _) = "Location"
getContentType (CommonMessage _ _ (Sticker _) _) = "Sticker"
getContentType (CommonMessage _ _ (Buttons _) _) = "Buttons"
getContentType _ = "No content"

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


data ProcessMessageResult = ProcessMessageResult
    {
        getUpdateId :: UpdateID
        , getLogMsg :: LogMessage
        , getUsers :: Users
        , getChatId :: Maybe Integer
        , getRepetition :: Maybe Int 
        , geMessage :: Maybe CMessage
        , getCaption :: Maybe T.Text
    }
    | Empty 

