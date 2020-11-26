module API.Telegram.Structs where

import qualified Data.Text as T 
import GHC.Generics




data TelegramUpdatesError = TelegramUpdatesError 
    {
        error_code:: Integer
        , description :: T.Text
    } deriving (Generic, Show)

data TelegramUpdates = TelegramUpdates 
    {
        ok :: Bool
        , result :: [TelUpdateResult]
    } deriving (Generic, Show)

data TelUpdateResult = TelUpdateResult 
    {
        update_id :: Integer
        , messageInfo ::Maybe  MessageInfo
        , callback_query :: Maybe Callback 
    } deriving Show 

data Callback = Callback 
    {
        cb_msg :: CBMsg
        , cb_data :: T.Text
    } deriving Show 

data CBMsg = CBMsg 
    {
        cb_chat :: CBChat
    } deriving Show 

data CBChat = CBChat
    {
        cb_chid :: Integer 
    } deriving Show 


data MessageInfo = MessageInfo 
    {
        message_id :: Integer 
        , chat :: TelChat
        , txt :: Maybe T.Text
        , animation :: Maybe TelAmination
        , audio :: Maybe TelAudio
        , document :: Maybe TelDocument
        , photo :: Maybe [TelPhoto]
        , video :: Maybe TelVideo
        , voice :: Maybe TelVoice
        , contact :: Maybe TelContact
        , poll :: Maybe TelPoll
        , venue :: Maybe TelVenue
        , location :: Maybe TelLocation
        , mediagroup :: Maybe T.Text
        , caption :: Maybe T.Text
        , sticker :: Maybe TelSticker
    } deriving Show 

data TelChat = TelChat
    {
        chat_id :: Integer
        ,chat_type :: T.Text 
    } deriving Show 

data TelForward = TelForward 
    {
        f_id :: Maybe Integer 
        , f_is_bot :: Maybe Bool
        , f_first_name :: Maybe T.Text
        , f_last_name :: Maybe T.Text
        , f_username :: Maybe T.Text
    } deriving Show 

data TelSticker = TelSticker
    {
        s_file_id :: T.Text
        , s_is_animated :: Bool 

    } deriving Show 

data TelAudio = TelAudio 
    {
        audio_id :: T.Text
        , audio_duration :: Maybe Integer
        , audio_performer :: Maybe T.Text
        , audio_title :: Maybe T.Text
    } deriving Show

data TelPhoto = TelPhoto 
    {
        photo_file_id :: T.Text
    } deriving Show

data TelDocument = TelDocument 
    {
         doc_file_id :: T.Text
    } deriving Show 

data TelLocation = TelLocation
    {
        latitude :: Double
        , longitude :: Double 
    } deriving (Generic, Show ) 

data TelVideo = TelVideo 
    {
        video_file_id :: T.Text
    } deriving Show 

data TelVoice = TelVoice
    {
        voice_file_id :: T.Text
    } deriving Show 

data TelAmination = TelAmination 
    {
        animation_file_id :: T.Text
    } deriving Show 

data TelContact = TelContact
    {
        phone_number :: T.Text
        , first_name :: T.Text
        , last_name :: Maybe T.Text
        , vcard :: Maybe T.Text
    } deriving (Show)

data TelPoll = TelPoll 
    {
        question :: T.Text 
        , poll_options :: [PollOptions]
        , is_anonymous :: Maybe Bool
        , poll_type :: Maybe T.Text
        , allows_multiple_answers :: Maybe Bool
        , correct_option_id :: Maybe Integer 
        , explanation :: Maybe T.Text
        , open_period :: Maybe Integer 
        , close_date :: Maybe Integer
        , is_closed :: Maybe Bool 
    } deriving Show 

data PollOptions = PollOptions 
    {
        poll_option :: T.Text
        , voter_count :: Integer 
    } deriving Show 

data TelVenue = TelVenue
    {
        v_latitude :: Double 
        , v_longitude :: Double
        , v_title :: T.Text
        , v_address :: T.Text
    } deriving Show 


data Button = Button T.Text T.Text
    deriving Show 

data InlineKeyBoard = InlineKeyBoard [[Button]]
    deriving Show 