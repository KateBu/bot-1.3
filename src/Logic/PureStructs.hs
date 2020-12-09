module Logic.PureStructs where

import qualified API.Telegram.Structs as TStructs 
import qualified Config.Config as Config 
import Logger.Logger ()

import qualified Data.Text as T 


type UpdateID = Int
type MbCaption = Maybe T.Text

data Message = EmptyMessage UpdateID
    | UserCommand UpdateID Command
    | CommonMessage UpdateID Int CMessage MbCaption
    | CallbackQuery UpdateID Int T.Text

data Command = Command 
    {
         chatID :: Int
        , text :: T.Text
    }

data CMessage = Txt  T.Text 
    | Animation  TStructs.TelAmination 
    | Audio  TStructs.TelAudio 
    | Document  TStructs.TelDocument 
    | Photo  [TStructs.TelPhoto] 
    | Video  TStructs.TelVideo 
    | Voice  TStructs.TelVoice 
    | Contact  TStructs.TelContact 
    | Poll  TStructs.TelPoll 
    | Venue  TStructs.TelVenue 
    | Location  TStructs.TelLocation 
    | Sticker  TStructs.TelSticker 
    | Buttons [[TStructs.Button]]
    | Other 

data ProcessMessageResult = ProcessMessageResult 
    {
        updID :: UpdateID
        , msgType :: T.Text
        , newConfig :: Config.Config
        , mbChatID :: Maybe Int
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

buttons :: [[TStructs.Button]]
buttons = [[TStructs.Button "1" "/setRepetition1"]
    , [TStructs.Button "2" "/setRepetition2"]
    , [TStructs.Button "3" "/setRepetition3"]
    , [TStructs.Button "4" "/setRepetition4"]
    , [TStructs.Button "5" "/setRepetition5"]]

repeatText :: T.Text
repeatText = "Выберите количество повторов: "

newRepeatText :: Int -> T.Text
newRepeatText rep = "Установлено количество обновлений: " <> (T.pack . show) rep

cMsgToText :: CMessage -> T.Text
cMsgToText (Txt txt) = "Text message: " <> txt 

