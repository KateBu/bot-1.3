module API.Telegram.Wrapper where

-- this file will be removed soon 

{-
import Data.Aeson ( Value, object, KeyValue((.=)), ToJSON(toJSON) )
import Data.Aeson.Types ( Pair )
import qualified Data.Text as T 

import qualified Config.Config as Config 
import qualified Logic.PureStructs as PureStructs 


newtype TButtons = TButtons PureStructs.PureButtons 

instance ToJSON TButtons where 
    toJSON (TButtons (PureStructs.PureButtons lbl txt)) = object ["text" .= lbl, "callback_data" .= txt] 

messageToPairs :: PureStructs.ComMessage -> [Pair]
messageToPairs cMsg = 
    getMaybeText "text" (PureStructs.mbText cMsg) 
    <> getMaybeText "animation" (PureStructs.mbAnimationFileId cMsg)
    <> getMaybeText "audio" (PureStructs.audioFileId <$> PureStructs.mbAudio cMsg)
    <> getMaybeVal "duration" (PureStructs.mbAudio cMsg >>= PureStructs.mbAudioDuration)
    <> getMaybeText "performer" (PureStructs.mbAudio cMsg >>= PureStructs.mbAudioPerformer)
    <> getMaybeText "title" (PureStructs.mbAudio cMsg >>= PureStructs.mbAudioTitle)
    <> getMaybeText "document" (PureStructs.mbDocFileId cMsg)
    <> getMaybeText "photo" (PureStructs.mbPhotoFileIds cMsg >>= Just . head)
    <> getMaybeText "video" (PureStructs.mbVideoFileId cMsg)
    <> getMaybeText "voice" (PureStructs.mbVoiceFileId cMsg)
    <> getMaybeText "phone_number" (PureStructs.contactPhoneNumber <$> PureStructs.mbContact cMsg)
    <> getMaybeText "first_name" (PureStructs.contactFirstName <$> PureStructs.mbContact cMsg)
    <> getMaybeText "last_name" (PureStructs.mbContact cMsg >>= PureStructs.mbContactLastName)
    <> getMaybeText "vcard" (PureStructs.mbContact cMsg >>= PureStructs.mbContactVCard)
    <> getMaybeText "question" (PureStructs.pollQuestion <$> PureStructs.mbPoll cMsg)
    <> pollOptionsToPair (PureStructs.pollOptions <$> PureStructs.mbPoll cMsg) 
    <> getMaybeVal "is_anonimous" (PureStructs.mbPoll cMsg >>= PureStructs.mbPollAnonymous)
    <> getMaybeText "type" (PureStructs.mbPoll cMsg >>= PureStructs.mbPollType)
    <> getMaybeVal "allows_multiple_answers" (PureStructs.mbPoll cMsg >>= PureStructs.mbPollAllowsMultiAnswers)
    <> getMaybeVal "correct_option_id" (PureStructs.mbPoll cMsg >>= PureStructs.mbPollCorrectId)
    <> getMaybeText "explanation" (PureStructs.mbPoll cMsg >>= PureStructs.mbPollExplanation)
    <> getMaybeVal "open_period" (PureStructs.mbPoll cMsg >>= PureStructs.mbPollOpenPeriod)
    <> getMaybeVal "close_date" (PureStructs.mbPoll cMsg >>= PureStructs.mbPollCloseDate)
    <> getMaybeVal "is_closed" (PureStructs.mbPoll cMsg >>= PureStructs.mbPollIsClosed)
    <> getMaybeVal "latitude" (PureStructs.venueLat <$> PureStructs.mbVenue cMsg)
    <> getMaybeVal "longitude" (PureStructs.venueLong <$> PureStructs.mbVenue cMsg)
    <> getMaybeVal "title" (PureStructs.venueTitle <$> PureStructs.mbVenue cMsg)
    <> getMaybeVal "address" (PureStructs.venueAddress <$> PureStructs.mbVenue cMsg)
    <> getMaybeVal "latitude" (PureStructs.locationLat <$> PureStructs.mbLocation cMsg)
    <> getMaybeVal "longitude" (PureStructs.locationLong <$> PureStructs.mbLocation cMsg)
    <> getMaybeText "sticker" (PureStructs.stickerFileId <$> PureStructs.mbSticker cMsg) 
    <> makeButtons (PureStructs.buttons cMsg)

makeButtons :: Bool -> [Pair]
makeButtons False = []
makeButtons _ = ["reply_markup" .= makeKeyboard ((map (map TButtons)) PureStructs.buttons')
    , "one_time_keyboard" .= True]

pollOptionsToPair :: Maybe [(T.Text, Int)] -> [Pair]
pollOptionsToPair Nothing = [] 
pollOptionsToPair (Just pollOptions) = ["options" .= map fst pollOptions]

makeKeyboard :: [[TButtons]] -> Value
makeKeyboard btns = object ["inline_keyboard" .= btns]

getMaybeText :: T.Text -> Maybe T.Text -> [Pair] 
getMaybeText prop (Just val) = [prop .= val]
getMaybeText _ _ = []

getMaybeVal :: Show a => T.Text -> Maybe a -> [Pair]
getMaybeVal prop (Just val) = [prop .= show val]
getMaybeVal _ _ = [] 

makeMessageObject :: PureStructs.Message -> Value
makeMessageObject (PureStructs.CommonMessage _ chid msg cap) = object $ 
    ["chat_id" .= chid]
    <> messageToPairs msg  
    <> getMaybeText "caption" cap 
makeMessageObject _ = undefined 

sendMessageHttpRequest :: Config.Config -> PureStructs.Message -> String 
sendMessageHttpRequest (Config.Config (Config.Telegram tok _) _ _ _ _) msg = case PureStructs.getContentType msg of 
    "No content" -> undefined
    _ -> "https://api.telegram.org/bot" 
        <> tok 
        <> "/send" 
        <> T.unpack (PureStructs.getContentType msg)
sendMessageHttpRequest _ _ = undefined         -}