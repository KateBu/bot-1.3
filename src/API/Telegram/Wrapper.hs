module API.Telegram.Wrapper where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T 

import Config.Config 
import Logic.PureStructs

import API.Telegram.Structs
import API.Telegram.Parsers



messageToPairs :: CMessage -> [Pair]
messageToPairs (Txt text ) = ["text" .= text] 
messageToPairs (Animation animation ) = ["animation" .= animation_file_id animation] 
messageToPairs (Audio audio) = ["audio" .= audio_id audio]
    <> getMaybeVal "duration" (audio_duration audio)
    <> getMaybeText "performer" (audio_performer audio)
    <> getMaybeText "title" (audio_title audio)
messageToPairs (Document document) = ["document" .= doc_file_id document]
messageToPairs (Photo photo ) = ["photo" .= (photo_file_id . head) photo]
messageToPairs (Video video ) = ["video" .= video_file_id video]
messageToPairs (Voice voice ) = ["voice" .= voice_file_id voice]
messageToPairs (Contact contact) = ["phone_number" .= phone_number contact
    , "first_name" .= first_name contact]
    <> getMaybeText "last_name" (last_name contact)
    <> getMaybeText "vcard" (vcard contact)
messageToPairs (Poll poll) = ["question" .= question poll
    , "options" .= (map poll_option (poll_options poll))]
    <> getMaybeVal "is_anonymous" (is_anonymous poll)
    <> getMaybeText "type" (poll_type poll) 
    <> getMaybeVal "allows_multiple_answers" (allows_multiple_answers poll)
    <> getMaybeVal "correct_option_id" (correct_option_id poll)
    <> getMaybeText "explanation" (explanation poll)
    <> getMaybeVal "open_period" (open_period poll)
    <> getMaybeVal "close_date" (close_date poll)
    <> getMaybeVal "is_closed" (is_closed poll)
messageToPairs (Venue venue ) = ["latitude" .= v_latitude venue
    , "longitude" .= v_longitude venue
    , "title" .= v_title venue
    , "address" .= v_address venue]
messageToPairs (Location location) = ["latitude" .= latitude location
    , "longitude" .= longitude location]
messageToPairs (Sticker sticker) = ["sticker" .= s_file_id sticker]
messageToPairs (Buttons btns) = ["text" .= repeatText
    ,"reply_markup" .= makeKeyboard btns
    ,"one_time_keyboard" .= True]


makeKeyboard :: [[Button]] -> Value
makeKeyboard btns = object ["inline_keyboard" .= btns]

getMaybeText :: T.Text -> Maybe T.Text -> [Pair] 
getMaybeText prop (Just val) = [prop .= val]
getMaybeText _ _ = []

getMaybeVal :: Show a => T.Text -> Maybe a -> [Pair]
getMaybeVal prop (Just val) = [prop .= show val]
getMaybeVal _ _ = [] 

makeMessageObject :: Message -> Value
makeMessageObject (CommonMessage _ chid msg cap) = object $ 
    ["chat_id" .= chid]
    <> messageToPairs msg  
    <> getMaybeText "caption" cap 

sendMessageHttpRequest :: Config -> Message -> String 
sendMessageHttpRequest (Config (Telegram tok _) _ _ _ _) msg = case getContentType msg of 
    "No content" -> undefined
    _ -> "https://api.telegram.org/bot" 
        <> tok 
        <> "/send" 
        <> T.unpack (getContentType msg)