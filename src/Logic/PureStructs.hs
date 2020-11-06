module Logic.PureStructs where

import API.Telegram.Structs
import Config.Config
import Logger.Logger

import qualified Data.Text as T 



data Updates = TUpdates TelegramUpdates | VKUpdates 

type UpdateID = Integer 
type MbCaption = Maybe T.Text


data Message = EmptyMessage UpdateID
    | UserCommand UpdateID Command
    | CommonMessage UpdateID Integer CMessage MbCaption
    | CallbackQuery UpdateID Integer T.Text

getUid :: Message -> UpdateID
getUid (EmptyMessage uid) = uid 
getUid (UserCommand uid _) = uid 
getUid (CommonMessage uid _ _ _) = uid 

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